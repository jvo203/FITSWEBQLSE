/* DISTRIBUTION STATEMENT A. Approved for public release; distribution is
 * unlimited.  Granted clearance per 88ABW-2015-5731.
 *
 * This file is declared a work of the U.S. Government and is not subject
 * to copyright protection in the United States.
 */

#include <errno.h>
#include <zmq.h>

/* Error numbers */
const int enotsup = ENOTSUP;
const int eprotonosupport = EPROTONOSUPPORT;
const int enobufs = ENOBUFS;
const int enetdown = ENETDOWN;
const int eaddrinuse = EADDRINUSE;
const int eaddrnotavail = EADDRNOTAVAIL;
const int econnrefused = ECONNREFUSED;
const int einprogress = EINPROGRESS;
const int enotsock = ENOTSOCK;
const int emsgsize = EMSGSIZE;
const int eafnosupport = EAFNOSUPPORT;
const int enetunreach = ENETUNREACH;
const int econnaborted = ECONNABORTED;
const int econnreset = ECONNRESET;
const int enotconn = ENOTCONN;
const int etimedout = ETIMEDOUT;
const int ehostunreach = EHOSTUNREACH;
const int enetreset = ENETRESET;
const int efsm = EFSM;
const int enocompatproto = ENOCOMPATPROTO;
const int eterm = ETERM;
const int emthread = EMTHREAD;

/* Version numbers */
const int zmq_version_major = ZMQ_VERSION_MAJOR;
const int zmq_version_minor = ZMQ_VERSION_MINOR;
const int zmq_version_patch = ZMQ_VERSION_PATCH;
const int fzmq_version_major = 0;
const int fzmq_version_minor = 8;
const int fzmq_version_patch = 0;

/* Context options */
const int zmq_io_theads = ZMQ_IO_THREADS;
const int zmq_max_sockets = ZMQ_MAX_SOCKETS;
const int zmq_socket_limit = ZMQ_SOCKET_LIMIT;
const int zmq_thread_priority = ZMQ_THREAD_PRIORITY;
const int zmq_thread_sched_policy = ZMQ_THREAD_SCHED_POLICY;

/* Context defaults */
const int zmq_io_threads_dflt = ZMQ_IO_THREADS_DFLT;
const int zmq_max_sockets_dflt = ZMQ_MAX_SOCKETS_DFLT;
const int zmq_thread_priority_dflt = ZMQ_THREAD_PRIORITY_DFLT;
const int zmq_thread_sched_policy_dflt = ZMQ_THREAD_SCHED_POLICY_DFLT;

/* Socket types */
const int zmq_pair = ZMQ_PAIR;
const int zmq_pub = ZMQ_PUB;
const int zmq_sub = ZMQ_SUB;
const int zmq_req = ZMQ_REQ;
const int zmq_rep = ZMQ_REP;
const int zmq_dealer = ZMQ_DEALER;
const int zmq_router = ZMQ_ROUTER;
const int zmq_pull = ZMQ_PULL;
const int zmq_push = ZMQ_PUSH;
const int zmq_xpub = ZMQ_XPUB;
const int zmq_xsub = ZMQ_XSUB;
const int zmq_stream = ZMQ_STREAM;

/* Socket options */
const int zmq_affinity = ZMQ_AFFINITY;
const int zmq_identity = ZMQ_IDENTITY;
const int zmq_subscribe = ZMQ_SUBSCRIBE;
const int zmq_unsubscribe = ZMQ_UNSUBSCRIBE;
const int zmq_rate = ZMQ_RATE;
const int zmq_recovery_ivl = ZMQ_RECOVERY_IVL;
const int zmq_sndbuf = ZMQ_SNDBUF;
const int zmq_rcvbuf = ZMQ_RCVBUF;
const int zmq_rcvmore = ZMQ_RCVMORE;
const int zmq_fd = ZMQ_FD;
const int zmq_events = ZMQ_EVENTS;
const int zmq_type = ZMQ_TYPE;
const int zmq_linger = ZMQ_LINGER;
const int zmq_reconnect_ivl = ZMQ_RECONNECT_IVL;
const int zmq_backlog = ZMQ_BACKLOG;
const int zmq_reconnect_ivl_max = ZMQ_RECONNECT_IVL_MAX;
const int zmq_maxmsgsize = ZMQ_MAXMSGSIZE;
const int zmq_sndhwm = ZMQ_SNDHWM;
const int zmq_rcvhwm = ZMQ_RCVHWM;
const int zmq_multicast_hops = ZMQ_MULTICAST_HOPS;
const int zmq_rcvtimeo = ZMQ_RCVTIMEO;
const int zmq_sndtimeo = ZMQ_SNDTIMEO;
const int zmq_last_endpoint = ZMQ_LAST_ENDPOINT;
const int zmq_router_mandatory = ZMQ_ROUTER_MANDATORY;
const int zmq_tcp_keepalive = ZMQ_TCP_KEEPALIVE;
const int zmq_tcp_keepalive_cnt = ZMQ_TCP_KEEPALIVE_CNT;
const int zmq_tcp_keepalive_idle = ZMQ_TCP_KEEPALIVE_IDLE;
const int zmq_tcp_keepalive_intvl = ZMQ_TCP_KEEPALIVE_INTVL;
const int zmq_immediate = ZMQ_IMMEDIATE;
const int zmq_xpub_verbose = ZMQ_XPUB_VERBOSE;
const int zmq_router_raw = ZMQ_ROUTER_RAW;
const int zmq_ipv6 = ZMQ_IPV6;
const int zmq_mechanism = ZMQ_MECHANISM;
const int zmq_plain_server = ZMQ_PLAIN_SERVER;
const int zmq_plain_username = ZMQ_PLAIN_USERNAME;
const int zmq_plain_password = ZMQ_PLAIN_PASSWORD;
const int zmq_curve_server = ZMQ_CURVE_SERVER;
const int zmq_curve_publickey = ZMQ_CURVE_PUBLICKEY;
const int zmq_curve_secretkey = ZMQ_CURVE_SECRETKEY;
const int zmq_curve_serverkey = ZMQ_CURVE_SERVERKEY;
const int zmq_probe_router = ZMQ_PROBE_ROUTER;
const int zmq_req_correlate = ZMQ_REQ_CORRELATE;
const int zmq_req_relaxed = ZMQ_REQ_RELAXED;
const int zmq_conflate = ZMQ_CONFLATE;
const int zmq_zap_domain = ZMQ_ZAP_DOMAIN;
const int zmq_router_handover = ZMQ_ROUTER_HANDOVER;
const int zmq_tos = ZMQ_TOS;
const int zmq_connect_rid = ZMQ_CONNECT_RID;
const int zmq_gssapi_server = ZMQ_GSSAPI_SERVER;
const int zmq_gssapi_principal = ZMQ_GSSAPI_PRINCIPAL;
const int zmq_gssapi_service_principal = ZMQ_GSSAPI_SERVICE_PRINCIPAL;
const int zmq_gssapi_plaintext = ZMQ_GSSAPI_PLAINTEXT;
const int zmq_handshake_ivl = ZMQ_HANDSHAKE_IVL;
const int zmq_socks_proxy = ZMQ_SOCKS_PROXY;
const int zmq_xpub_nodrop = ZMQ_XPUB_NODROP;

/* Message options */
const int zmq_more = ZMQ_MORE;
const int zmq_srcfd = ZMQ_SRCFD;
const int zmq_shared = ZMQ_SHARED;

/* Send/Recv options */
const int zmq_dontwait = ZMQ_DONTWAIT;
const int zmq_sndmore = ZMQ_SNDMORE;

/* Security mechanisms */
const int zmq_null = ZMQ_NULL;
const int zmq_plain = ZMQ_PLAIN;
const int zmq_curve = ZMQ_CURVE;
const int zmq_gssapi = ZMQ_GSSAPI;

/* Events */
const int zmq_event_connected = ZMQ_EVENT_CONNECTED;
const int zmq_event_connect_delayed = ZMQ_EVENT_CONNECT_DELAYED;
const int zmq_event_connect_retried = ZMQ_EVENT_CONNECT_RETRIED;
const int zmq_event_listening = ZMQ_EVENT_LISTENING;
const int zmq_event_bind_failed = ZMQ_EVENT_BIND_FAILED;
const int zmq_event_accepted = ZMQ_EVENT_ACCEPTED;
const int zmq_event_accept_failed = ZMQ_EVENT_ACCEPT_FAILED;
const int zmq_event_closed = ZMQ_EVENT_CLOSED;
const int zmq_event_close_failed = ZMQ_EVENT_CLOSE_FAILED;
const int zmq_event_disconnected = ZMQ_EVENT_DISCONNECTED;
const int zmq_event_monitor_stopped = ZMQ_EVENT_MONITOR_STOPPED;
const int zmq_event_all = ZMQ_EVENT_ALL;

/* Polling options */
const int zmq_pollin = ZMQ_POLLIN;
const int zmq_pollout = ZMQ_POLLOUT;
const int zmq_pollerr = ZMQ_POLLERR;

/* Probe capabilities */
const int zmq_has_capabilities = ZMQ_HAS_CAPABILITIES;

/* Deprecated */
const int zmq_xreq = ZMQ_XREQ;
const int zmq_xrep = ZMQ_XREP;
const int zmq_tcp_accept_filter = ZMQ_TCP_ACCEPT_FILTER;
const int zmq_ipc_filter_pid = ZMQ_IPC_FILTER_PID;
const int zmq_ipc_filter_uid = ZMQ_IPC_FILTER_UID;
const int zmq_ipc_filter_gid = ZMQ_IPC_FILTER_GID;
const int zmq_ipv4only = ZMQ_IPV4ONLY;
const int zmq_delay_attach_on_connect = ZMQ_DELAY_ATTACH_ON_CONNECT;
const int zmq_noblock = ZMQ_NOBLOCK;
const int zmq_fail_unroutable = ZMQ_FAIL_UNROUTABLE;
const int zmq_router_behavior = ZMQ_ROUTER_BEHAVIOR;
const int zmq_streamer = ZMQ_STREAMER;
const int zmq_forwarder = ZMQ_FORWARDER;
const int zmq_queue = ZMQ_QUEUE;
