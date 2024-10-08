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

#include <pthread.h>

extern int get_physical_cores();

#ifdef MEMDEBUG
#include <jemalloc/jemalloc.h>

static pthread_t jemalloc_t;
static void *jemalloc_daemon(void *arg);
#endif

#include <x265.h>

#include "ini.h"
#include "http.h"
#include "ws.h"

#include "mongoose.h"
#include "mjson.h"

#include "cluster.h"
#include "hash_table.h"

#include "version.h"

#if !defined(__APPLE__) || !defined(__MACH__)
#include <ipp.h>
// #include <ippcore_tl.h>
#endif

#include <curl/curl.h>
#include <glib.h>

static volatile sig_atomic_t s_received_signal = 0;

static void signal_handler(int sig_num)
{
    printf("[C] Interrupt signal [%d] received.\n", sig_num);

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
static pthread_t garbage_collection_t;

static void *autodiscovery_daemon(void *);
static void *garbage_collection_daemon(void *);
extern void init_fortran();
extern void cleanup_fortran();

#define OPTSTR "c:d:p:h"
#define USAGE_FMT "%s  [-c config file] [-d home directory] [-p HTTP port] [-h]\n"
#define DEFAULT_PROGNAME "fitswebql"

void usage(char *progname, int opt);

static int handler(void *user, const char *section, const char *name,
                   const char *value);

options_t options; // the one and only one definition

#if !defined(__APPLE__) || !defined(__MACH__)
void ipp_init()
{
    IppStatus status;
    Ipp64u mask, emask;

    /* Init IPP library */
    ippInit();

    /* Get IPP library version info */
    const IppLibraryVersion *pVer = ippsGetLibVersion();

    /* Get IPP library threading model */
    /*IppThreadingType thrType;
    ippGetThreadingType_LT(&thrType);
    printf("Intel(R) IPP Threading Layer (%s)", (thrType == OMP) ? "OpenMP" : "TBB");*/

    printf("Intel(R) IPP: %s %s %s", pVer->Name, pVer->Version, pVer->BuildDate);
    printf("\n");

    return;

    /* Get CPU features and features enabled with selected library level */
    status = ippGetCpuFeatures(&mask, 0);
    if (ippStsNoErr == status)
    {
        emask = ippGetEnabledCpuFeatures();
        printf("[C] Features supported by CPU\tby IPP\n");
        printf("-----------------------------------------\n");
        printf("  ippCPUID_MMX        = ");
        printf("%c\t%c\t", (mask & ippCPUID_MMX) ? 'Y' : 'N',
               (emask & ippCPUID_MMX) ? 'Y' : 'N');
        printf("Intel(R) Architecture MMX technology supported\n");
        printf("  ippCPUID_SSE        = ");
        printf("%c\t%c\t", (mask & ippCPUID_SSE) ? 'Y' : 'N',
               (emask & ippCPUID_SSE) ? 'Y' : 'N');
        printf("Intel(R) Streaming SIMD Extensions\n");
        printf("  ippCPUID_SSE2       = ");
        printf("%c\t%c\t", (mask & ippCPUID_SSE2) ? 'Y' : 'N',
               (emask & ippCPUID_SSE2) ? 'Y' : 'N');
        printf("Intel(R) Streaming SIMD Extensions 2\n");
        printf("  ippCPUID_SSE3       = ");
        printf("%c\t%c\t", (mask & ippCPUID_SSE3) ? 'Y' : 'N',
               (emask & ippCPUID_SSE3) ? 'Y' : 'N');
        printf("Intel(R) Streaming SIMD Extensions 3\n");
        printf("  ippCPUID_SSSE3      = ");
        printf("%c\t%c\t", (mask & ippCPUID_SSSE3) ? 'Y' : 'N',
               (emask & ippCPUID_SSSE3) ? 'Y' : 'N');
        printf("Intel(R) Supplemental Streaming SIMD Extensions 3\n");
        printf("  ippCPUID_MOVBE      = ");
        printf("%c\t%c\t", (mask & ippCPUID_MOVBE) ? 'Y' : 'N',
               (emask & ippCPUID_MOVBE) ? 'Y' : 'N');
        printf("The processor supports MOVBE instruction\n");
        printf("  ippCPUID_SSE41      = ");
        printf("%c\t%c\t", (mask & ippCPUID_SSE41) ? 'Y' : 'N',
               (emask & ippCPUID_SSE41) ? 'Y' : 'N');
        printf("Intel(R) Streaming SIMD Extensions 4.1\n");
        printf("  ippCPUID_SSE42      = ");
        printf("%c\t%c\t", (mask & ippCPUID_SSE42) ? 'Y' : 'N',
               (emask & ippCPUID_SSE42) ? 'Y' : 'N');
        printf("Intel(R) Streaming SIMD Extensions 4.2\n");
        printf("  ippCPUID_AVX        = ");
        printf("%c\t%c\t", (mask & ippCPUID_AVX) ? 'Y' : 'N',
               (emask & ippCPUID_AVX) ? 'Y' : 'N');
        printf("Intel(R) Advanced Vector Extensions instruction set\n");
        printf("  ippAVX_ENABLEDBYOS  = ");
        printf("%c\t%c\t", (mask & ippAVX_ENABLEDBYOS) ? 'Y' : 'N',
               (emask & ippAVX_ENABLEDBYOS) ? 'Y' : 'N');
        printf("The operating system supports Intel(R) AVX\n");
        printf("  ippCPUID_AES        = ");
        printf("%c\t%c\t", (mask & ippCPUID_AES) ? 'Y' : 'N',
               (emask & ippCPUID_AES) ? 'Y' : 'N');
        printf("Intel(R) AES instruction\n");
        printf("  ippCPUID_SHA        = ");
        printf("%c\t%c\t", (mask & ippCPUID_SHA) ? 'Y' : 'N',
               (emask & ippCPUID_SHA) ? 'Y' : 'N');
        printf("Intel(R) SHA new instructions\n");
        printf("  ippCPUID_CLMUL      = ");
        printf("%c\t%c\t", (mask & ippCPUID_CLMUL) ? 'Y' : 'N',
               (emask & ippCPUID_CLMUL) ? 'Y' : 'N');
        printf("PCLMULQDQ instruction\n");
        printf("  ippCPUID_RDRAND     = ");
        printf("%c\t%c\t", (mask & ippCPUID_RDRAND) ? 'Y' : 'N',
               (emask & ippCPUID_RDRAND) ? 'Y' : 'N');
        printf("Read Random Number instructions\n");
        printf("  ippCPUID_F16C       = ");
        printf("%c\t%c\t", (mask & ippCPUID_F16C) ? 'Y' : 'N',
               (emask & ippCPUID_F16C) ? 'Y' : 'N');
        printf("Float16 instructions\n");
        printf("  ippCPUID_AVX2       = ");
        printf("%c\t%c\t", (mask & ippCPUID_AVX2) ? 'Y' : 'N',
               (emask & ippCPUID_AVX2) ? 'Y' : 'N');
        printf("Intel(R) Advanced Vector Extensions 2 instruction set\n");
        printf("  ippCPUID_AVX512F    = ");
        printf("%c\t%c\t", (mask & ippCPUID_AVX512F) ? 'Y' : 'N',
               (emask & ippCPUID_AVX512F) ? 'Y' : 'N');
        printf("Intel(R) Advanced Vector Extensions 3.1 instruction set\n");
        printf("  ippCPUID_AVX512CD   = ");
        printf("%c\t%c\t", (mask & ippCPUID_AVX512CD) ? 'Y' : 'N',
               (emask & ippCPUID_AVX512CD) ? 'Y' : 'N');
        printf("Intel(R) Advanced Vector Extensions CD (Conflict Detection) "
               "instruction set\n");
        printf("  ippCPUID_AVX512ER   = ");
        printf("%c\t%c\t", (mask & ippCPUID_AVX512ER) ? 'Y' : 'N',
               (emask & ippCPUID_AVX512ER) ? 'Y' : 'N');
        printf("Intel(R) Advanced Vector Extensions ER instruction set\n");
        printf("  ippCPUID_ADCOX      = ");
        printf("%c\t%c\t", (mask & ippCPUID_ADCOX) ? 'Y' : 'N',
               (emask & ippCPUID_ADCOX) ? 'Y' : 'N');
        printf("ADCX and ADOX instructions\n");
        printf("  ippCPUID_RDSEED     = ");
        printf("%c\t%c\t", (mask & ippCPUID_RDSEED) ? 'Y' : 'N',
               (emask & ippCPUID_RDSEED) ? 'Y' : 'N');
        printf("The RDSEED instruction\n");
        printf("  ippCPUID_PREFETCHW  = ");
        printf("%c\t%c\t", (mask & ippCPUID_PREFETCHW) ? 'Y' : 'N',
               (emask & ippCPUID_PREFETCHW) ? 'Y' : 'N');
        printf("The PREFETCHW instruction\n");
        printf("  ippCPUID_KNC        = ");
        printf("%c\t%c\t", (mask & ippCPUID_KNC) ? 'Y' : 'N',
               (emask & ippCPUID_KNC) ? 'Y' : 'N');
        printf("Intel(R) Xeon Phi(TM) Coprocessor instruction set\n");
    }
}
#endif

int main(int argc, char *argv[])
{
#ifdef MEMDEBUG
    int jemalloc_res = pthread_create(&jemalloc_t, NULL, jemalloc_daemon, NULL);

    if (jemalloc_res)
    {
        printf("error %d\n", jemalloc_res);
        exit(EXIT_FAILURE);
    }
#endif

    printf("%s %s\n", SERVER_STRING, VERSION_STRING);

    struct passwd *passwdEnt = getpwuid(getuid());

    // default initial values
    options.http_port = 8080;
    options.ws_port = options.http_port + 1;
    options.local = true;
    options.timeout = 15; // [s]
#if !defined(__APPLE__) || !defined(__MACH__)
    options.fits_home = strdup(".cache");
    options.cache = strdup(".cache");
    options.logs = strdup("LOGS");
#else
    char cache[1024];

    snprintf(cache, sizeof(cache), "%s/.cache", passwdEnt->pw_dir);
    options.fits_home = strdup(cache);
    options.cache = strdup(cache);

    strcat(cache, "/LOGS");
    options.logs = strdup(cache);
#endif
    options.threshold = 25; // [GiB]
    options.home_dir = strdup(passwdEnt->pw_dir);

    options.user = strdup("jvo");
    options.password = NULL;
    options.host = strdup("localhost");
    options.port = 5432;
    options.db_home = strdup("/home");

    options.url_protocol = strdup("http");
    options.url_host = strdup("localhost");
    options.url_port = 80;

    options.root = NULL;

    // if the port is 0 the cluster auto-discovery will be disabled
    // the port can be set in the config.ini file
    options.zmq_port = 0; // was BEACON_PORT;

    // parse options command-line options (over-rides the .ini config file)
    int opt;

    bool port_override = false;
    uint16_t port_number = 8080;
    char *config_file = strdup("config.ini");

    while ((opt = getopt(argc, argv, OPTSTR)) != EOF)
        switch (opt)
        {
        case 'p':
            options.http_port = (uint16_t)strtoul(optarg, NULL, 10);
            options.ws_port = options.http_port + 1;

            // save the command-line port
            port_override = true;
            port_number = options.http_port;

            break;

        case 'c':
            free(config_file);
            config_file = strdup(optarg);

            break;

        case 'd':
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
        printf("Home Directory: '%s'.\n", options.home_dir);

    // parse a config file
    if (ini_parse(config_file, handler, &options) < 0)
        printf("Can't load '%s', assuming default options.\n", config_file);
    else
        printf("Successfully parsed '%s'.\n", config_file);

    printf("Cache Directory: '%s'. Please clean it periodically to prevent excessive disk usage.\n", options.cache);

    // fortran
    {
        init_fortran();

        mkcache(options.logs);

        printf("Log Directory: '%s'.\n", options.logs);

        /*GString *log_file = g_string_new(options.logs);

        g_string_append(log_file, "/fortran.log");

        FILE *fp;
        fp = fopen(log_file->str, "w");
        if (fp != NULL)
            fclose(fp);

        g_string_free(log_file, TRUE);*/
    }

    // a manual port override
    if (port_override)
    {
        options.http_port = port_number;
        options.ws_port = options.http_port + 1;
    }

#if !defined(__APPLE__) || !defined(__MACH__)
    ipp_init();
#endif

    curl_global_init(CURL_GLOBAL_ALL);

    init_cluster();
    init_hash_table();
    init_session_table();

    // ZeroMQ node auto-discovery
    setenv("ZSYS_SIGHANDLER", "false", 1);

    int res = 0;

    if (options.zmq_port == 0)
        printf("ZeroMQ node auto-discovery is disabled. To enable the cluster functionality please specify a non-zero port in 'config.ini'.\n");
    else
        res = pthread_create(&zmq_t, NULL, autodiscovery_daemon, NULL);

    if (res)
    {
        printf("error %d\n", res);
        exit(EXIT_FAILURE);
    }

    res = pthread_create(&garbage_collection_t, NULL, garbage_collection_daemon, NULL);

    if (res)
    {
        printf("error %d\n", res);
        exit(EXIT_FAILURE);
    }

    printf("Browser URL: http://localhost:%" PRIu16 "\n", options.http_port);
    printf("*** To quit FITSWEBQLSE press Ctrl-C from the command-line terminal or send SIGINT. ***\n");

    // Ctrl-C signal handler
    signal(SIGTERM, signal_handler);
    signal(SIGINT, signal_handler);

    start_http();

    while (s_received_signal == 0)
    {
        sleep(1);
    }

    // stop accepting new connections
    quiesce_http();

    // close all websocket sessions
    int no_sessions = close_sessions();

    if (no_sessions > 0)
    {
        printf("[C] sleep(10 seconds); waiting for any remaining sessions to close...\n");
        sleep(10);
    }

    stop_http();

    // clean-up ZeroMQ
    if (speaker != NULL)
    {
        zstr_sendx(speaker, "SILENCE", NULL);

        // leave the cluster
        const char *msg_leave = "LEAVE";
        const int interval = 1000; //[ms]
        zsock_send(speaker, "sbi", "PUBLISH", msg_leave, strlen(msg_leave), interval);

        // shutdown the cluster
        const char *msg_exit = "SHUTDOWN";
        zsock_send(speaker, "sbi", "PUBLISH", msg_exit, strlen(msg_exit), interval);

        // stop broadcasting the beacon
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

    pthread_join(garbage_collection_t, NULL);

    delete_cluster();

    delete_hash_table();

    delete_session_table();

    curl_global_cleanup();

    cleanup_fortran();

    // release any memory allocated in options
    free(options.fits_home);
    free(options.cache);
    free(options.logs);
    free(options.home_dir);
    free(options.user);
    free(options.password);
    free(options.host);
    free(options.db_home);
    free(options.root);
    free(config_file);
    free(options.url_protocol);
    free(options.url_host);

    x265_cleanup();

#ifdef MEMDEBUG
    // wait for the jemalloc thread
    pthread_join(jemalloc_t, NULL);
#endif

    return EXIT_SUCCESS;
}

void usage(char *progname, int opt)
{
    (void)opt;
    fprintf(stderr, USAGE_FMT, progname ? progname : DEFAULT_PROGNAME);
    exit(EXIT_SUCCESS);
    /* NOTREACHED */
}

static int handler(void *user, const char *section, const char *name,
                   const char *value)
{
    options_t *options = (options_t *)user;

#define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0

    if (MATCH("fitswebql", "local"))
    {
        if (strcmp(value, "true") == 0)
            options->local = true;

        if (strcmp(value, "false") == 0)
            options->local = false;
    }
    else if (MATCH("fitswebql", "timeout"))
    {
        options->timeout = atoi(value);
    }
    else if (MATCH("fitswebql", "home"))
    {
        free(options->fits_home);
        options->fits_home = strdup(value);
    }
    else if (MATCH("fitswebql", "logs"))
    {
        free(options->logs);
        options->logs = strdup(value);
    }
    else if (MATCH("fitswebql", "cache"))
    {
        free(options->cache);
        options->cache = strdup(value);
    }
    else if (MATCH("fitswebql", "threshold"))
    {
        options->threshold = atoi(value);
    }
    else if (MATCH("fitswebql", "port"))
    {
        options->http_port = atoi(value);
        options->ws_port = options->http_port + 1;
    }
    else if (MATCH("postgresql", "host"))
    {
        free(options->host);
        options->host = strdup(value);
    }
    else if (MATCH("postgresql", "user"))
    {
        free(options->user);
        options->user = strdup(value);
    }
    else if (MATCH("postgresql", "password"))
    {
        free(options->password);
        options->password = strdup(value);
    }
    else if (MATCH("postgresql", "home"))
    {
        free(options->db_home);
        options->db_home = strdup(value);
    }
    else if (MATCH("postgresql", "port"))
    {
        options->port = atoi(value);
    }
    else if (MATCH("download", "protocol"))
    {
        free(options->url_protocol);
        options->url_protocol = strdup(value);
    }
    else if (MATCH("download", "host"))
    {
        free(options->url_host);
        options->url_host = strdup(value);
    }
    else if (MATCH("download", "port"))
    {
        options->url_port = atoi(value);
    }
    else if (MATCH("zeromq", "port"))
    {
        options->zmq_port = atoi(value);
    }
    else
    {
        printf("unknown option %s/%s\n", section, name);
        return 0; /* unknown section/name, error */
    }
    return 1;
}

int gstrcmp(const void *pa, const void *pb)
{
    /* compare strings */
    return strcmp((const char *)pa, (const char *)pb);
}

static void *autodiscovery_daemon(void *ptr)
{
    (void)ptr;

    speaker = zactor_new(zbeacon, NULL);
    if (speaker == NULL)
        pthread_exit(NULL);

#ifdef DEBUG
    zstr_send(speaker, "VERBOSE");
#endif
    zsock_send(speaker, "si", "CONFIGURE", options.zmq_port);
    char *my_hostname = zstr_recv(speaker);

    if (my_hostname != NULL)
    {
        const char *message = "ENTER";
        const int interval = 1000; //[ms]
        zsock_send(speaker, "sbi", "PUBLISH", message, strlen(message), interval);

        options.root = strdup(my_hostname);
#ifdef DEBUG
        printf("<options.root>: %s\n", options.root);
#endif
    }

    listener = zactor_new(zbeacon, NULL);
    if (listener == NULL)
        pthread_exit(NULL);

#ifdef DEBUG
    zstr_send(listener, "VERBOSE");
#endif
    zsock_send(listener, "si", "CONFIGURE", options.zmq_port);
    char *hostname = zstr_recv(listener);
    if (hostname != NULL)
        free(hostname);
    else
        pthread_exit(NULL);

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
                        free(item->data);

                        // remove an item from the list
                        cluster = g_slist_delete_link(cluster, item);

                        printf("[ØMQ] removed '%s' from the cluster.\n", ipaddress);
                    }

                    g_mutex_unlock(&cluster_mtx);
                }
            }

            // SHUTDOWN
            if (strstr(msg, "SHUTDOWN") != NULL)
            {
                if (strcmp(my_hostname, ipaddress) != 0)
                {
                    printf("[ØMQ] received '%s' from %s\n", msg, ipaddress);

                    // raise SIGINT
                    int ret = raise(SIGINT);

                    if (ret != 0)
                        printf("[C] Error: unable to raise SIGINT signal.\n");
                }
            }

            free(msg);

            zframe_destroy(&content);
            zstr_free(&ipaddress);
        }
    }

    free(my_hostname);

    pthread_exit(NULL);
}

static void *garbage_collection_daemon(void *ptr)
{
    (void)ptr;

    // disable garbage collection if timeout == 0
    if (options.timeout == 0)
    {
        printf("timeout = %" PRIu32 ", disabling dataset garbage collection.\n", options.timeout);
        pthread_exit(NULL);
    }

    while (s_received_signal == 0)
    {
        garbage_collect();
        sleep(1);
    }

    printf("garbage collection thread terminated.\n");

    pthread_exit(NULL);
}

#ifdef MEMDEBUG
static void *jemalloc_daemon(void *arg)
{
    (void)arg;

    printf("jemalloc memory tracking thread initiated.\n");

    FILE *fp = fopen("memory_usage.csv", "w");

    if (fp != NULL)
        fprintf(fp,
                "\"elapsed time "
                "[s]\",\"stats.allocated\",\"stats.active\",\"stats.mapped\"\n");
    else
    {
        printf("cannot open 'memory_usage.csv' for writing.\n");
        pthread_exit(NULL);
    }

    time_t offset = time(NULL);

    while (s_received_signal == 0)
    {
        // memory statistics using jemalloc
        uint64_t epoch = 1;
        size_t sz = sizeof(epoch);
        mallctl("thread.tcache.flush", NULL, NULL, NULL, 0);
        mallctl("epoch", &epoch, &sz, &epoch, sz);

        size_t allocated, active, mapped;
        sz = sizeof(size_t);
        mallctl("stats.allocated", &allocated, &sz, NULL, 0);
        mallctl("stats.active", &active, &sz, NULL, 0);
        mallctl("stats.mapped", &mapped, &sz, NULL, 0);

        time_t now = time(NULL);
        double elapsed = difftime(now, offset);

        fprintf(fp, "%f,%zu,%zu,%zu\n", elapsed, allocated, active, mapped);

        sleep(1);
    }

    fclose(fp);

    printf("jemalloc memory tracking thread terminated.\n");

    pthread_exit(NULL);
}
#endif