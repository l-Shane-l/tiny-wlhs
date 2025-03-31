#ifndef SURFACE_H
#define SURFACE_H

#include "layer.h"
#include "server.h"
#include <wayland-server-core.h>
#include <wlr/types/wlr_xdg_activation_v1.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_shell.h>

void focus_layer_surface(struct tinywl_server *server,
                         struct wlr_surface *surface);

void layer_surface_map(struct wl_listener *listener, void *data);
void layer_surface_unmap(struct wl_listener *listener, void *data);
void layer_surface_configure(struct wl_listener *listener, void *data);
void layer_surface_destroy(struct wl_listener *listener, void *data);
struct wlr_surface *layer_surface_at(struct tinywl_server *server, double lx,
                                     double ly, double *sx, double *sy);
void server_new_layer_surface(struct wl_listener *listener, void *data);
static void apply_layer_surface_anchoring(struct tinywl_layer_surface *surface);
void server_new_xdg_surface(struct wl_listener *listener, void *data);

#endif
