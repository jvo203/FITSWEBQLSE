/*
 * ws protocol handler plugin for "lws-minimal"
 *
 * Written in 2010-2019 by Andy Green <andy@warmcat.com>
 *
 * This file is made available under the Creative Commons CC0 1.0
 * Universal Public Domain Dedication.
 *
 * This version uses an lws_ring ringbuffer to cache up to 8 messages at a time,
 * so it's not so easy to lose messages.
 *
 * This also demonstrates how to "cull", ie, kill, connections that can't
 * keep up for some reason.
 */

#if !defined(LWS_PLUGIN_STATIC)
#define LWS_DLL
#define LWS_INTERNAL
#include <libwebsockets.h>
#endif

#include "json.h"

#include <string.h>
#include <stdbool.h>

#define RING_DEPTH 128

enum zoom_shape
{
	circle,
	square
};

enum intensity_mode
{
	mean,
	integrated
};

enum image_quality
{
	low,
	medium,
	high
};

enum request_type
{
	realtime_image_spectrum,
	kalman_init,
	kalman_reset
};

struct image_spectrum_request
{
	// input
	int dx;
	bool image;
	enum image_quality quality;
	int x1;
	int y1;
	int x2;
	int y2;
	int width;
	int height;
	enum zoom_shape beam;
	enum intensity_mode intensity;
	double frame_start;
	double frame_end;
	double ref_freq;
	int seq_id;
	float timestamp;
	char *datasetid;

	// output
	int fd;
};

extern void realtime_image_spectrum_request(char *datasetid, size_t n, void *req);

void *launch_image_spectrum_request(void *ptr)
{
	if (ptr == NULL)
		return NULL;

	struct image_spectrum_request *req = (struct image_spectrum_request *)ptr;

	// pass the request to FORTRAN
	realtime_image_spectrum_request(req->datasetid, strlen(req->datasetid), req);

	if (req->datasetid != NULL)
		free(req->datasetid);

	free(req);

	return NULL;
}

/* one of these created for each message */

struct msg
{
	void *payload; /* is malloc'd */
	size_t len;
	bool binary;
};

/* one of these is created for each client connecting to us */

struct per_session_data__minimal
{
	struct per_session_data__minimal *pss_list;
	struct lws *wsi;
	uint32_t tail;

	// FITSWEBQL-related
	char *datasetid;
	struct image_spectrum_request is_req;
	enum request_type req_type;
	bool new_request;

	unsigned int culled : 1;
};

/* one of these is created for each vhost our protocol is used with */

struct per_vhost_data__minimal
{
	struct lws_context *context;
	struct lws_vhost *vhost;
	const struct lws_protocols *protocol;

	struct per_session_data__minimal *pss_list; /* linked-list of live pss*/

	struct lws_ring *ring; /* ringbuffer holding unsent messages */
};

static void
cull_lagging_clients(struct per_vhost_data__minimal *vhd)
{
	uint32_t oldest_tail = lws_ring_get_oldest_tail(vhd->ring);
	struct per_session_data__minimal *old_pss = NULL;
	int most = 0, before = (int)lws_ring_get_count_waiting_elements(vhd->ring, &oldest_tail), m;

	/*
	 * At least one guy with the oldest tail has lagged too far, filling
	 * the ringbuffer with stuff waiting for them, while new stuff is
	 * coming in, and they must close, freeing up ringbuffer entries.
	 */

	lws_start_foreach_llp_safe(struct per_session_data__minimal **,
							   ppss, vhd->pss_list, pss_list)
	{

		if ((*ppss)->tail == oldest_tail)
		{
			old_pss = *ppss;

			lwsl_user("Killing lagging client %p\n", (*ppss)->wsi);

			lws_set_timeout((*ppss)->wsi, PENDING_TIMEOUT_LAGGING,
							/*
					 * we may kill the wsi we came in on,
					 * so the actual close is deferred
					 */
							LWS_TO_KILL_ASYNC);

			/*
			 * We might try to write something before we get a
			 * chance to close.  But this pss is now detached
			 * from the ring buffer.  Mark this pss as culled so we
			 * don't try to do anything more with it.
			 */

			(*ppss)->culled = 1;

			/*
			 * Because we can't kill it synchronously, but we
			 * know it's closing momentarily and don't want its
			 * participation any more, remove its pss from the
			 * vhd pss list early.  (This is safe to repeat
			 * uselessly later in the close flow).
			 *
			 * Notice this changes *ppss!
			 */

			lws_ll_fwd_remove(struct per_session_data__minimal,
							  pss_list, (*ppss), vhd->pss_list);

			/* use the changed *ppss so we won't skip anything */

			continue;
		}
		else
		{
			/*
			 * so this guy is a survivor of the cull.  Let's track
			 * what is the largest number of pending ring elements
			 * for any survivor.
			 */
			m = (int)lws_ring_get_count_waiting_elements(vhd->ring,
														 &((*ppss)->tail));
			if (m > most)
				most = m;
		}
	}
	lws_end_foreach_llp_safe(ppss);

	/* it would mean we lost track of oldest... but Coverity insists */
	if (!old_pss)
		return;

	/*
	 * Let's recover (ie, free up) all the ring slots between the
	 * original oldest's last one and the "worst" survivor.
	 */

	lws_ring_consume_and_update_oldest_tail(vhd->ring,
											struct per_session_data__minimal, &old_pss->tail, (size_t)(before - most),
											vhd->pss_list, tail, pss_list);

	lwsl_user("%s: shrunk ring from %d to %d\n", __func__, before, most);
}

/* destroys the message when everyone has had a copy of it */

static void
__minimal_destroy_message(void *_msg)
{
	struct msg *msg = _msg;

	free(msg->payload);
	msg->payload = NULL;
	msg->len = 0;
}

static int
callback_minimal(struct lws *wsi, enum lws_callback_reasons reason,
				 void *user, void *in, size_t len)
{
	struct per_session_data__minimal *pss =
		(struct per_session_data__minimal *)user;
	struct per_vhost_data__minimal *vhd =
		(struct per_vhost_data__minimal *)
			lws_protocol_vh_priv_get(lws_get_vhost(wsi),
									 lws_get_protocol(wsi));
	const struct msg *pmsg;
	struct msg amsg;
	int n, m;

	// JVO additions
	uint8_t buf[LWS_PRE + 2048], *start = &buf[LWS_PRE], *p = start,
								 *end = &buf[sizeof(buf) - LWS_PRE - 1];

	char *ws_msg;
	char *ptr;
	enum lws_write_protocol mode;

	switch (reason)
	{
	case LWS_CALLBACK_PROTOCOL_INIT:
		vhd = lws_protocol_vh_priv_zalloc(lws_get_vhost(wsi),
										  lws_get_protocol(wsi),
										  sizeof(struct per_vhost_data__minimal));
		vhd->context = lws_get_context(wsi);
		vhd->protocol = lws_get_protocol(wsi);
		vhd->vhost = lws_get_vhost(wsi);

		vhd->ring = lws_ring_create(sizeof(struct msg), RING_DEPTH,
									__minimal_destroy_message);
		if (!vhd->ring)
			return 1;
		break;

	case LWS_CALLBACK_PROTOCOL_DESTROY:
		lws_ring_destroy(vhd->ring);
		break;

	case LWS_CALLBACK_ESTABLISHED:
		n = lws_hdr_copy(wsi, buf, sizeof(buf), WSI_TOKEN_GET_URI);
		lwsl_notice("[ws] %s\n", (const char *)buf);

		// find the last slash
		ptr = strrchr((const char *)buf, '/');

		// reject connections without a datasetid
		if (ptr == NULL)
			return -1; // forcibly close the connection

		/* add ourselves to the list of live pss held in the vhd */
		// lwsl_user("LWS_CALLBACK_ESTABLISHED: wsi %p\n", wsi);

		lws_ll_fwd_insert(pss, pss_list, vhd->pss_list);
		pss->tail = lws_ring_get_oldest_tail(vhd->ring);
		pss->wsi = wsi;
		pss->datasetid = strdup(ptr + 1);
		lwsl_user("[ws] CONNECTION ESTABLISHED FOR %s\n", pss->datasetid);
		break;

	case LWS_CALLBACK_CLOSED:
		// lwsl_user("LWS_CALLBACK_CLOSED: wsi %p\n", wsi);
		/* remove our closing pss from the list of live pss */
		lws_ll_fwd_remove(struct per_session_data__minimal, pss_list,
						  pss, vhd->pss_list);
		lwsl_user("[ws] CONNECTION CLOSED FOR %s\n", pss->datasetid);
		free(pss->datasetid);
		break;

	case LWS_CALLBACK_SERVER_WRITEABLE:
		if (pss->culled)
			break;

		// is there a new request to be passed to Fortran
		if (pss->new_request)
		{
			lwsl_user("[ws] NEW REQUEST: %d\n", pss->req_type);

			if (pss->req_type == realtime_image_spectrum)
			{
				int status;
				int pipefd[2];
				ssize_t n = 0;
				size_t offset = 0;

				char *buf = NULL;
				size_t buf_size = 0x2000;
				//char buf[0x40000];

				uint32_t length, view_width, view_height;
				uint32_t compressed_size;
				size_t msg_len, view_size;

				printf("[C] dx:%d, image:%d, quality:%d, x1:%d, y1:%d, x2:%d, y2:%d, width:%d, height:%d, beam:%d, intensity:%d, frame_start:%f, frame_end:%f, ref_freq:%f, seq_id:%d, timestamp:%f\n", pss->is_req.dx, pss->is_req.image, pss->is_req.quality, pss->is_req.x1, pss->is_req.y1, pss->is_req.x2, pss->is_req.y2, pss->is_req.width, pss->is_req.height, pss->is_req.beam, pss->is_req.intensity, pss->is_req.frame_start, pss->is_req.frame_end, pss->is_req.ref_freq, pss->is_req.seq_id, pss->is_req.timestamp);

				// open a pipe
				status = pipe(pipefd);

				if (0 == status)
				{
					struct image_spectrum_request *req = (struct image_spectrum_request *)malloc(sizeof(struct image_spectrum_request));

					if (req != NULL)
					{
						int stat;
						pthread_t for_tid;

						// first copy the contents
						memcpy(req, &(pss->is_req), sizeof(struct image_spectrum_request));

						// add the datasetid to be passed to FORTRAN
						req->datasetid = strdup(pss->datasetid);

						// pass the write end of the pipe
						req->fd = pipefd[1];

						// a FORTRAN launch thread
						// on success the thread will close the write end of the pipe
						stat = pthread_create(&for_tid, NULL, launch_image_spectrum_request, req);

						if (0 == stat)
						{
							buf = malloc(buf_size);

							if (buf != NULL)
								while ((n = read(pipefd[0], buf + offset, buf_size - offset)) > 0)
								{
									offset += n;

									printf("[C] PIPE_RECV %zd BYTES, OFFSET: %zu, buf_size: %zu\n", n, offset, buf_size);

									if (offset == buf_size)
									{
										printf("[C] OFFSET == BUF_SIZE, re-sizing the buffer\n");

										size_t new_size = buf_size << 1;
										char *tmp = realloc(buf, new_size);

										if (tmp != NULL)
										{
											buf = tmp;
											buf_size = new_size;
										}
									}
								}

							if (0 == n)
								printf("[C] PIPE_END_OF_STREAM\n");

							if (n < 0)
								printf("[C] PIPE_END_WITH_ERROR\n");

							stat = pthread_join(for_tid, NULL);

							length = 0;
							compressed_size = 0;

							// process the received data, prepare WebSocket response(s)
							if (offset > 8)
							{
								memcpy(&length, buf, sizeof(uint32_t));
								memcpy(&compressed_size, buf + 4, sizeof(uint32_t));

								printf("[C] length: %u, compressed_size: %u\n", length, compressed_size);

								// set the binary mode
								amsg.binary = true;

								msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(float) + compressed_size;

								amsg.len = msg_len;

								/* notice we over-allocate by LWS_PRE... */
								amsg.payload = malloc(LWS_PRE + msg_len);

								if (amsg.payload != NULL)
								{
									float ts = pss->is_req.timestamp;
									uint32_t id = pss->is_req.seq_id;
									uint32_t msg_type = 0;
									// 0 - spectrum, 1 - viewport,
									// 2 - image, 3 - full, spectrum,  refresh,
									// 4 - histogram
									float elapsed = 0.0f;

									/* ...and we copy the payload in at +LWS_PRE */
									size_t ws_offset = LWS_PRE;

									memcpy((char *)amsg.payload + ws_offset, &ts, sizeof(float));
									ws_offset += sizeof(float);

									memcpy((char *)amsg.payload + ws_offset, &id, sizeof(uint32_t));
									ws_offset += sizeof(uint32_t);

									memcpy((char *)amsg.payload + ws_offset, &msg_type, sizeof(uint32_t));
									ws_offset += sizeof(uint32_t);

									memcpy((char *)amsg.payload + ws_offset, &elapsed, sizeof(float));
									ws_offset += sizeof(float);

									memcpy((char *)amsg.payload + ws_offset, buf + 8, compressed_size);
									ws_offset += compressed_size;

									if (!lws_ring_insert(vhd->ring, &amsg, 1))
									{
										__minimal_destroy_message(&amsg);
										lwsl_user("dropping!\n");
									}
								}
								else
									lwsl_user("OOM: skipping spectrum\n");

								// check if there is an optional viewport too
								if (offset > 8 + compressed_size + 8)
								{
									memcpy(&view_width, buf + 8 + compressed_size, sizeof(uint32_t));
									memcpy(&view_height, buf + 8 + compressed_size + 4, sizeof(uint32_t));
									view_size = offset - (8 + compressed_size);

									if (view_width > 0 && view_height > 0)
									{
										lwsl_user("processing %dx%d viewport.\n", view_width, view_height);

										// set the binary mode
										amsg.binary = true;

										// header
										msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(float);
										// body
										msg_len += view_size;

										amsg.len = msg_len;

										/* notice we over-allocate by LWS_PRE... */
										amsg.payload = malloc(LWS_PRE + msg_len);

										if (amsg.payload != NULL)
										{
											float ts = pss->is_req.timestamp;
											uint32_t id = pss->is_req.seq_id;
											uint32_t msg_type = 1;
											// 0 - spectrum, 1 - viewport,
											// 2 - image, 3 - full, spectrum,  refresh,
											// 4 - histogram
											float elapsed = 0.0f;

											/* ...and we copy the payload in at +LWS_PRE */
											size_t ws_offset = LWS_PRE;

											memcpy((char *)amsg.payload + ws_offset, &ts, sizeof(float));
											ws_offset += sizeof(float);

											memcpy((char *)amsg.payload + ws_offset, &id, sizeof(uint32_t));
											ws_offset += sizeof(uint32_t);

											memcpy((char *)amsg.payload + ws_offset, &msg_type, sizeof(uint32_t));
											ws_offset += sizeof(uint32_t);

											memcpy((char *)amsg.payload + ws_offset, &elapsed, sizeof(float));
											ws_offset += sizeof(float);

											memcpy((char *)amsg.payload + ws_offset, buf + 8 + compressed_size, view_size);
											ws_offset += view_size;

											if (!lws_ring_insert(vhd->ring, &amsg, 1))
											{
												__minimal_destroy_message(&amsg);
												lwsl_user("dropping!\n");
											}
										}
										else
											lwsl_user("OOM: skipping viewport\n");
									}
								}
							}

							if (buf != NULL)
								free(buf);
						}
						else
						{
							// close the write end of the pipe
							close(pipefd[1]);
						}
					}
					else
					{
						// close the write end of the pipe
						close(pipefd[1]);
					}

					// close the read end of the pipe
					close(pipefd[0]);
				}

				// reset the pipe
				pss->is_req.fd = -1;
			}

			pss->new_request = false;
		}

		pmsg = lws_ring_get_element(vhd->ring, &pss->tail);
		if (!pmsg)
			break;

		mode = pmsg->binary ? LWS_WRITE_BINARY : LWS_WRITE_TEXT;

		/* notice we allowed for LWS_PRE in the payload already */
		m = lws_write(wsi, ((unsigned char *)pmsg->payload) + LWS_PRE, pmsg->len, mode);
		if (m < (int)pmsg->len)
		{
			lwsl_err("ERROR %d writing to ws socket\n", m);
			return -1;
		}

		lws_ring_consume_and_update_oldest_tail(
			vhd->ring,						  /* lws_ring object */
			struct per_session_data__minimal, /* type of objects with tails */
			&pss->tail,						  /* tail of guy doing the consuming */
			1,								  /* number of payload objects being consumed */
			vhd->pss_list,					  /* head of list of objects with tails */
			tail,							  /* member name of tail in objects with tails */
			pss_list						  /* member name of next object in objects with tails */
		);

		/* more to do for us? */
		if (lws_ring_get_element(vhd->ring, &pss->tail))
			/* come back as soon as we can write more */
			lws_callback_on_writable(pss->wsi);
		break;

	case LWS_CALLBACK_RECEIVE:
		n = (int)lws_ring_get_count_free_elements(vhd->ring);
		if (!n)
		{
			/* forcibly make space */
			cull_lagging_clients(vhd);
			n = (int)lws_ring_get_count_free_elements(vhd->ring);
		}
		if (!n)
			break;

		//lwsl_user("LWS_CALLBACK_RECEIVE: free space %d\n", n);

		ws_msg = malloc(len + 1);

		if (ws_msg != NULL)
		{
			memcpy(ws_msg, in, len);
			ws_msg[len] = '\0';
			lwsl_user("[ws] MESSAGE RECEIVED: %s.\n", ws_msg);
			// printf("[ws] (%s)\n", ws_msg);
		}
		else
		{
			lwsl_user("OOM: dropping\n");
			break;
		}

		// parse JSON
		JsonNode *json = json_decode(ws_msg);

		if (json != NULL)
		{
			lwsl_user("[ws] JSON parsed OK.\n");

			JsonNode *node = NULL;

			json_foreach(node, json)
			{
				char *key = node->key;

				// if (key != NULL)
				//	printf("%s\t", key);

				if (strcmp(key, "type") == 0 && node->tag == JSON_STRING)
				{
					// test for realtime_image_spectrum
					if (strcmp(node->string_, "realtime_image_spectrum") == 0)
					{
						pss->req_type = realtime_image_spectrum;
						pss->new_request = true;
					}

					// test for kalman_init
					if (strcmp(node->string_, "kalman_init") == 0)
					{
						pss->req_type = kalman_init;
						pss->new_request = true;
					}

					// test for kalman_reset
					if (strcmp(node->string_, "kalman_reset") == 0)
					{
						pss->req_type = kalman_reset;
						pss->new_request = true;
					}
				}

				if (strcmp(key, "dx") == 0 && node->tag == JSON_NUMBER)
					pss->is_req.dx = node->number_;

				if (strcmp(key, "image") == 0 && node->tag == JSON_BOOL)
					pss->is_req.image = node->bool_;

				if (strcmp(key, "quality") == 0 && node->tag == JSON_STRING)
				{
					// test for low
					if (strcmp(node->string_, "low") == 0)
						pss->is_req.quality = low;

					// test for medium
					if (strcmp(node->string_, "medium") == 0)
						pss->is_req.quality = medium;

					// test for high
					if (strcmp(node->string_, "high") == 0)
						pss->is_req.quality = high;
				}

				if (strcmp(key, "x1") == 0 && node->tag == JSON_NUMBER)
					pss->is_req.x1 = node->number_;

				if (strcmp(key, "y1") == 0 && node->tag == JSON_NUMBER)
					pss->is_req.y1 = node->number_;

				if (strcmp(key, "x2") == 0 && node->tag == JSON_NUMBER)
					pss->is_req.x2 = node->number_;

				if (strcmp(key, "y2") == 0 && node->tag == JSON_NUMBER)
					pss->is_req.y2 = node->number_;

				if (strcmp(key, "width") == 0 && node->tag == JSON_NUMBER)
					pss->is_req.width = node->number_;

				if (strcmp(key, "height") == 0 && node->tag == JSON_NUMBER)
					pss->is_req.height = node->number_;

				if (strcmp(key, "beam") == 0 && node->tag == JSON_STRING)
				{
					// test for circle
					if (strcmp(node->string_, "circle") == 0)
						pss->is_req.beam = circle;

					// test for square
					if (strcmp(node->string_, "square") == 0)
						pss->is_req.beam = square;
				}

				if (strcmp(key, "intensity") == 0 && node->tag == JSON_STRING)
				{
					// test for mean
					if (strcmp(node->string_, "mean") == 0)
						pss->is_req.intensity = mean;

					// test for integrated
					if (strcmp(node->string_, "integrated") == 0)
						pss->is_req.intensity = integrated;
				}

				if (strcmp(key, "frame_start") == 0 && node->tag == JSON_NUMBER)
					pss->is_req.frame_start = node->number_;

				if (strcmp(key, "frame_end") == 0 && node->tag == JSON_NUMBER)
					pss->is_req.frame_end = node->number_;

				if (strcmp(key, "ref_freq") == 0 && node->tag == JSON_NUMBER)
					pss->is_req.ref_freq = node->number_;

				if (strcmp(key, "seq_id") == 0 && node->tag == JSON_NUMBER)
					pss->is_req.seq_id = node->number_;

				if (strcmp(key, "timestamp") == 0 && node->tag == JSON_NUMBER)
					pss->is_req.timestamp = node->number_;
			}

			// release memory
			json_delete(json);

			// stop here
			free(ws_msg);

			/*
		 	* let everybody know we want to write something on them
		 	* as soon as they are ready
		 	*/
			lws_start_foreach_llp(struct per_session_data__minimal **,
								  ppss, vhd->pss_list)
			{
				lws_callback_on_writable((*ppss)->wsi);
			}
			lws_end_foreach_llp(ppss, pss_list);
			break;
		}

		// if we got here it is because the message was not in a JSON format
		// only ping back heartbeats
		ptr = strstr((char *)ws_msg, "[heartbeat]");

		if (ptr == NULL)
		{
			free(ws_msg);
			break;
		}

		// ws_msg is no longer needed
		free(ws_msg);

		// set the text mode
		amsg.binary = false;

		amsg.len = len;
		/* notice we over-allocate by LWS_PRE... */
		amsg.payload = malloc(LWS_PRE + len);
		if (!amsg.payload)
		{
			lwsl_user("OOM: dropping\n");
			break;
		}

		/* ...and we copy the payload in at +LWS_PRE */
		memcpy((char *)amsg.payload + LWS_PRE, in, len);
		if (!lws_ring_insert(vhd->ring, &amsg, 1))
		{
			__minimal_destroy_message(&amsg);
			lwsl_user("dropping!\n");
			break;
		}

		/*
		 * let everybody know we want to write something on them
		 * as soon as they are ready
		 */
		lws_start_foreach_llp(struct per_session_data__minimal **,
							  ppss, vhd->pss_list)
		{
			lws_callback_on_writable((*ppss)->wsi);
		}
		lws_end_foreach_llp(ppss, pss_list);
		break;

	default:
		break;
	}

	return 0;
}

#define LWS_PLUGIN_PROTOCOL_MINIMAL                   \
	{                                                 \
		"fitswebqlse",                                \
			callback_minimal,                         \
			sizeof(struct per_session_data__minimal), \
			0,                                        \
			0, NULL, 0                                \
	}
