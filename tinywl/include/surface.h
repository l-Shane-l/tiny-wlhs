#ifndef SURFACE_H
#define SURFACE_H

#include "server.h"
#include <wayland-server-core.h>

void focus_layer_surface(struct tinywl_server *server,
                         struct wlr_surface *surface);

void layer_surface_map(struct wl_listener *listener, void *data);
void layer_surface_unmap(struct wl_listener *listener, void *data);
void layer_surface_configure(struct wl_listener *listener, void *data);
void layer_surface_destroy(struct wl_listener *listener, void *data);

#endif
