/* main.c */
/* FITSWEBQLSE by Christopher Zapart @ Japanese Virtual Observatory (JVO) in NAOJ */
/* chris.zapart@nao.ac.jp */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <unistd.h>
#include <getopt.h>
#include <libgen.h>
#include <string.h>
#include <pwd.h>

#include "ini.h"
#include "http.h"

#include <pthread.h>

#include "mongoose.h"
#include "mjson.h"

#include "cluster.h"
#include "hash_table.h"

#include "version.h"

#include <glib.h>

static volatile sig_atomic_t s_received_signal = 0;

static void signal_handler(int sig_num)
{
    printf("Interrupt signal [%d] received.\n", sig_num);

    signal(sig_num, signal_handler);
    s_received_signal = sig_num;
}

/* ZeroMQ node auto-discovery */
#define BEACON_PORT 50000

#include <czmq.h>

extern GSList *cluster;
extern GMutex cluster_mtx;

static zactor_t *speaker = NULL;
static zactor_t *listener = NULL;
static pthread_t zmq_t;

static void *autodiscovery_daemon(void *);

#define OPTSTR "p:h"
#define USAGE_FMT "%s [-p HTTP port] [-d home directory] [-h]\n"
#define DEFAULT_PROGNAME "fitswebql"

void usage(char *progname, int opt);

static int handler(void *user, const char *section, const char *name,
                   const char *value);

// HTTP request callback
static void mg_request_callback(struct mg_connection *c, int ev, void *ev_data, void *fn_data);

// Pipe event handler
static void mg_pipe_event_handler(struct mg_connection *c, int ev, void *ev_data, void *fn_data);

// Thread utility function
static void start_thread(void (*f)(void *), void *p);

options_t options; // the one and only one definition

int main(int argc, char *argv[])
{
    init_cluster();
    init_hash_table();

    // ZeroMQ node auto-discovery
    setenv("ZSYS_SIGHANDLER", "false", 1);

    int res = pthread_create(&zmq_t, NULL, autodiscovery_daemon, NULL);

    if (res)
    {
        printf("error %d\n", res);
        exit(EXIT_FAILURE);
    }

    printf("%s %s\n", SERVER_STRING, VERSION_STRING);

    struct passwd *passwdEnt = getpwuid(getuid());

    //options = {8080, 8081, true, false, 15, strdup(".cache"), strdup(".cache"), strdup("LOGS"), strdup(passwdEnt->pw_dir), strdup("jvo"), NULL, strdup("p10.vo.nao.ac.jp"), 5433, strdup("/home")}; // default values
    options.http_port = 8080;
    options.ws_port = options.http_port + 1;
    options.local = true;
    options.production = false;
    options.timeout = 15;
    options.fits_home = strdup(".cache");
    options.cache = strdup(".cache");
    options.logs = strdup("LOGS");
    options.home_dir = strdup(passwdEnt->pw_dir);

    options.user = strdup("jvo");
    options.password = NULL;
    options.host = strdup("p10.vo.nao.ac.jp");
    options.port = 5433;
    options.db_home = strdup("/home");

    // parse a config.ini config file
    if (ini_parse("config.ini", handler, &options) < 0)
        printf("Can't load 'config.ini', assuming default options.\n");
    else
        printf("Successfully parsed 'config.ini'.\n");

    // parse options command-line options (over-rides the .ini config file)
    int opt;

    while ((opt = getopt(argc, argv, OPTSTR)) != EOF)
        switch (opt)
        {
        case 'p':
            options.http_port = (uint16_t)strtoul(optarg, NULL, 10);
            options.ws_port = options.http_port + 1;
            break;

        case 'd':
            if (options.home_dir != NULL)
                free(options.home_dir);

            options.home_dir = strdup(optarg);
            break;

        case 'h':
        default:
            usage(basename(argv[0]), opt);
            /* NOTREACHED */
            break;
        }

    if (options.local)
        printf("Home Directory: %s\n", options.home_dir);

    printf("Browser URL: http://localhost:%" PRIu16 "\n", options.http_port);
    printf("*** To quit FITSWEBQLSE press Ctrl-C from the command-line terminal or send SIGINT. ***\n");

    // Ctrl-C signal handler
    // ignore SIGPIPE
    signal(SIGTERM, signal_handler);
    signal(SIGINT, signal_handler);

    start_http(options.http_port);

    // a mongoose server
    char url[256] = "";
    sprintf(url, "0.0.0.0:%d", options.ws_port);

    struct mg_mgr mgr;
    struct mg_connection *pipe; // Used to wake up event manager
    mg_mgr_init(&mgr);
    mg_log_set("3");
    pipe = mg_mkpipe(&mgr, mg_pipe_event_handler, NULL);  // Create pipe
    mg_http_listen(&mgr, url, mg_request_callback, pipe); // Create listener

    // a mongoose event loop
    while (s_received_signal == 0)
        mg_mgr_poll(&mgr, 1000); // Event loop

    mg_mgr_free(&mgr); // Cleanup

    stop_http();

    // shutdown the cluster
    distributed_exit();

    // release any memory allocated in options (really not needed at this point but ...)
    if (options.fits_home != NULL)
        free(options.fits_home);

    if (options.cache != NULL)
        free(options.cache);

    if (options.logs != NULL)
        free(options.logs);

    if (options.home_dir != NULL)
        free(options.home_dir);

    if (options.user != NULL)
        free(options.user);

    if (options.password != NULL)
        free(options.password);

    if (options.host != NULL)
        free(options.host);

    if (options.db_home != NULL)
        free(options.db_home);

    // clean-up ZeroMQ
    if (speaker != NULL)
    {
        zstr_sendx(speaker, "SILENCE", NULL);

        const char *message = "FITSWEBQLSE::LEAVE";
        const int interval = 1000; //[ms]
        zsock_send(speaker, "sbi", "PUBLISH", message, strlen(message), interval);

        zstr_sendx(speaker, "SILENCE", NULL);
        zactor_destroy(&speaker);
    }

    if (listener != NULL)
    {
        zstr_sendx(listener, "UNSUBSCRIBE", NULL);

        // wait for the ZeroMQ thread to exit
        pthread_join(zmq_t, NULL);

        zactor_destroy(&listener);
    }

    delete_cluster();
    delete_hash_table();

    return EXIT_SUCCESS;
}

void usage(char *progname, int opt)
{
    fprintf(stderr, USAGE_FMT, progname ? progname : DEFAULT_PROGNAME);
    exit(EXIT_FAILURE);
    /* NOTREACHED */
}

static int handler(void *user, const char *section, const char *name,
                   const char *value)
{
    options_t *options = (options_t *)user;

#define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0

    if (MATCH("fitswebql", "local"))
    {
        if (MATCH(value, "true"))
            options->local = true;

        if (MATCH(value, "false"))
            options->local = false;
    }
    else if (MATCH("fitswebql", "production"))
    {
        if (MATCH(value, "true"))
            options->production = true;

        if (MATCH(value, "false"))
            options->production = false;
    }
    else if (MATCH("fitswebql", "timeout"))
    {
        options->timeout = atoi(value);
    }
    else if (MATCH("fitswebql", "home"))
    {
        if (options->fits_home != NULL)
            free(options->fits_home);

        options->fits_home = strdup(value);
    }
    else if (MATCH("fitswebql", "logs"))
    {
        if (options->logs != NULL)
            free(options->logs);

        options->logs = strdup(value);
    }
    else if (MATCH("fitswebql", "cache"))
    {
        if (options->cache != NULL)
            free(options->cache);

        options->cache = strdup(value);
    }
    else if (MATCH("fitswebql", "port"))
    {
        options->http_port = atoi(value);
        options->ws_port = options->http_port + 1;
    }
    else if (MATCH("postgresql", "host"))
    {
        if (options->host != NULL)
            free(options->host);

        options->host = strdup(value);
    }
    else if (MATCH("postgresql", "user"))
    {
        if (options->user != NULL)
            free(options->user);

        options->user = strdup(value);
    }
    else if (MATCH("postgresql", "password"))
    {
        if (options->password != NULL)
            free(options->password);

        options->password = strdup(value);
    }
    else if (MATCH("postgresql", "home"))
    {
        if (options->db_home != NULL)
            free(options->db_home);

        options->db_home = strdup(value);
    }
    else if (MATCH("postgresql", "port"))
    {
        options->port = atoi(value);
    }
    else
    {
        printf("unknown option %s/%s\n", section, name);
        return 0; /* unknown section/name, error */
    }
    return 1;
}

static void start_thread(void (*f)(void *), void *p)
{
    pthread_t thread_id = (pthread_t)0;
    pthread_attr_t attr;
    (void)pthread_attr_init(&attr);
    (void)pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_create(&thread_id, &attr, (void *(*)(void *))f, p);
    pthread_attr_destroy(&attr);
}

static void thread_function(void *param)
{
    struct mg_connection *c = param; // Pipe connection
    sleep(2);                        // Simulate long execution
    mg_mgr_wakeup(c);                // Wakeup event manager
}

// HTTP request callback
static void mg_request_callback(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    if (ev == MG_EV_HTTP_MSG)
    {
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;
        if (mg_http_match_uri(hm, "/fast"))
        {
            // Single-threaded code path, for performance comparison
            // The /fast URI responds immediately
            mg_http_reply(c, 200, "Host: foo.com\r\n", "hi\n");
        }
        else
        {
            // Multithreading code path
            c->label[0] = 'W';                      // Mark us as waiting for data
            start_thread(thread_function, fn_data); // Start handling thread
        }
    }
}

// Pipe event handler
static void mg_pipe_event_handler(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    if (ev == MG_EV_READ)
    {
        struct mg_connection *t;
        for (t = c->mgr->conns; t != NULL; t = t->next)
        {
            if (t->label[0] != 'W')
                continue;                                       // Ignore un-marked connections
            mg_http_reply(t, 200, "Host: foo.com\r\n", "hi\n"); // Respond!
            t->label[0] = 0;                                    // Clear mark
        }
    }
}

int gstrcmp(const void *pa, const void *pb)
{
    /* compare strings */
    return strcmp((const char *)pa, (const char *)pb);
}

static void *autodiscovery_daemon(void *ptr)
{
    speaker = zactor_new(zbeacon, NULL);
    if (speaker == NULL)
        return NULL;

    zstr_send(speaker, "VERBOSE");
    zsock_send(speaker, "si", "CONFIGURE", BEACON_PORT);
    char *my_hostname = zstr_recv(speaker);

    if (my_hostname != NULL)
    {
        const char *message = "FITSWEBQLSE::ENTER";
        const int interval = 1000; //[ms]
        zsock_send(speaker, "sbi", "PUBLISH", message, strlen(message), interval);
    }

    listener = zactor_new(zbeacon, NULL);
    if (listener == NULL)
        return NULL;

    zstr_send(listener, "VERBOSE");
    zsock_send(listener, "si", "CONFIGURE", BEACON_PORT);
    char *hostname = zstr_recv(listener);
    if (hostname != NULL)
        free(hostname);
    else
        return NULL;

    zsock_send(listener, "sb", "SUBSCRIBE", "", 0);
    zsock_set_rcvtimeo(listener, 500);

    while (s_received_signal == 0)
    {
        char *ipaddress = zstr_recv(listener);
        if (ipaddress != NULL)
        {
            zframe_t *content = zframe_recv(listener);
            char *msg = strndup((const char *)zframe_data(content), zframe_size(content));

            // ENTER
            if (strstr(msg, "ENTER") != NULL)
            {
                if (strcmp(my_hostname, ipaddress) != 0)
                {
                    g_mutex_lock(&cluster_mtx);
                    GSList *item = g_slist_find_custom(cluster, ipaddress, gstrcmp);

                    // only insert if not present
                    if (item == NULL)
                    {
                        printf("[ØMQ] received '%s' from %s\n", msg, ipaddress);

                        // use prepend to avoid traversing to the end of the list
                        cluster = g_slist_prepend(cluster, strdup(ipaddress));

                        printf("[ØMQ] added '%s' to the cluster.\n", ipaddress);
                    }

                    g_mutex_unlock(&cluster_mtx);
                }
            }

            // LEAVE
            if (strstr(msg, "LEAVE") != NULL)
            {
                if (strcmp(my_hostname, ipaddress) != 0)
                {
                    g_mutex_lock(&cluster_mtx);
                    GSList *item = g_slist_find_custom(cluster, ipaddress, gstrcmp);

                    // only remove if present
                    if (item != NULL)
                    {
                        printf("[ØMQ] received '%s' from %s\n", msg, ipaddress);

                        // free the underlying data
                        if (item->data != NULL)
                            free(item->data);

                        // remove an item from the list
                        cluster = g_slist_delete_link(cluster, item);

                        printf("[ØMQ] removed '%s' from the cluster.\n", ipaddress);
                    }

                    g_mutex_unlock(&cluster_mtx);
                }
            }

            if (msg != NULL)
                free(msg);

            zframe_destroy(&content);
            zstr_free(&ipaddress);
        }
    }

    if (my_hostname != NULL)
        free(my_hostname);

    return NULL;
}