#ifndef SEAT_H
#define SEAT_H

#include "layer.h"
#include <stdint.h>

void seat_request_cursor(struct wl_listener *listener, void *data);
void seat_request_set_selection(struct wl_listener *listener, void *data);
void initialize_seat(struct tinywl_server *server);

#endif
