#ifndef OUTPUT_H
#define OUTPUT_H

#include "wayland-server-core.h"
#include "wayland-util.h"

struct tinywl_output {
  struct wl_list link;
  struct tinywl_server *server;
  struct wlr_output *wlr_output;
  struct wl_listener frame;
  struct wl_listener request_state;
  struct wl_listener destroy;
};

void get_output_dimensions(struct wlr_output *output, int32_t *width,
                           int32_t *height);

#endif
