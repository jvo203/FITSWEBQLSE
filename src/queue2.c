#include <sttdef.h>

// Single producer, single consumer non-blocking queue
//
// Producer:
//    void *buf;
//    while (mg_queue_space(q, &buf, len) == 0) WAIT();  // Wait for free space
//    memcpy(buf, data, len);  // Copy data to the queue
//    mg_queue_add(q, len);    // Advance q->head
//
// Consumer:
//    void *buf;
//    while ((len = mg_queue_next(q, &buf)) == 0) WAIT();
//    mg_hexdump(buf, len);    // Handle message
//    mg_queue_del(q);         // Delete message (advance tail)
//
struct mg_queue {
  char *buf;
  size_t size;
  volatile size_t tail;
  volatile size_t head;
};

void mg_queue_init(struct mg_queue *, char *, size_t);  // Init queue
void mg_queue_add(struct mg_queue *, size_t len);       // Advance head
void mg_queue_del(struct mg_queue *);                   // Advance tail
size_t mg_queue_next(struct mg_queue *, char **);       // Get next message size
size_t mg_queue_space(struct mg_queue *, char **, size_t);  // Get free space

#if defined( __GNUC__ ) || defined(__clang__)
#define MG_MEMORY_BARRIER() __sync_synchronize()
#else
#define MG_MEMORY_BARRIER()
#endif

// Every message in a queue is prepended by a 32-bit message length (ML).
// If ML is 0, then it is the end, and reader must wrap to the beginning.
//
//  Queue when q->tail <= q->head:
//  |----- free -----| ML | message1 | ML | message2 |  ----- free ------|
//  ^                ^                               ^                   ^
// buf              tail                            head                len
//
//  Queue when q->tail > q->head:
//  | ML | message2 |----- free ------| ML | message1 | 0 |---- free ----|
//  ^               ^                 ^                                  ^
// buf             head              tail                               len

void mg_queue_init(struct mg_queue *q, char *buf, size_t size) {
  q->size = size;
  q->buf = buf;
  q->head = q->tail = 0;
}

size_t mg_queue_space(struct mg_queue *q, char **buf, size_t min) {
  size_t space = 0, hs = sizeof(uint32_t) * 2;  // *2 is for the 0 marker
  if (q->head < q->tail) {                      // Head wrapped over?
    if (q->head + hs < q->tail) space = q->tail - q->head - hs;  // Yeah
  } else {
    if (q->head + min + hs <= q->size) {  // Enough space at the end?
      space = q->size - q->head - hs;     // Yeah. Report space
    } else {                              // Nope, must wrap
      if (q->head + sizeof(uint32_t) > q->size) MG_ERROR(("Q2Q2Q2"));
      memset(q->buf + q->head, 0, sizeof(uint32_t));  // Write zero marker
      if (q->tail > hs) space = q->tail - hs;
      MG_MEMORY_BARRIER();
      q->head = 0;  // Wrap head
    }
  }
  // printf("-->spc: %zu %3zu %zu %zu\n", q->tail, q->head, min, space);
  if (buf != NULL) *buf = q->buf + q->head + sizeof(uint32_t);
  return space;
}

size_t mg_queue_next(struct mg_queue *q, char **buf) {
  uint32_t len = 0;
  if (q->tail != q->head) {
    memcpy(&len, q->buf + q->tail, sizeof(len));  // Read length
    if (len == 0) {                               // Zero (head wrapped) ?
      q->tail = 0;                                // Reset tail to the start
      if (q->head > q->tail) memcpy(&len, q->buf, sizeof(len));  // Read again
    }
  }
  if (buf != NULL) *buf = q->buf + q->tail + sizeof(uint32_t);
  if (q->tail + len > q->size) {
    MG_ERROR(("  OOPS %u %u %u", (unsigned) q->tail, (unsigned) q->head, len));
    mg_hexdump(q->buf + q->tail, 4);
  }
  // printf("-->nxt: %3d %3d %lu\n", q->tail, q->head, len);
  return len;
}

void mg_queue_add(struct mg_queue *q, size_t len) {
  if (len > 0) {
    uint32_t n = (uint32_t) len;
    if (q->head + sizeof(uint32_t) * 2 + len > q->size) MG_ERROR(("QQQQQ"));
    memcpy(q->buf + q->head, &n, sizeof(n));
    MG_MEMORY_BARRIER();
    q->head += len + sizeof(n);
  }
  // printf("-->add: %3d %3d %lu\n", q->tail, q->head, len
}

void mg_queue_del(struct mg_queue *q) {
  size_t len = mg_queue_next(q, NULL);
  if (len > 0) q->tail += len + sizeof(uint32_t);
  // printf("-->del: %3d %3d\n", q->tail, q->head);
}
