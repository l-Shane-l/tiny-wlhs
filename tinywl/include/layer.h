#ifndef LAYER_H
#define LAYER_H

#include "wayland-server-core.h"
#include "wayland-util.h"
struct tinywl_layer_surface {
  struct wl_list link;
  struct tinywl_server *server;
  struct wlr_layer_surface_v1 *layer_surface;
  struct wlr_scene_layer_surface_v1 *scene_tree;

  struct wl_listener destroy;
  struct wl_listener map;
  struct wl_listener unmap;
  struct wl_listener configure; // Committed surface state changes

  // Optional additional state storage
  uint32_t width, height;
  bool mapped;
};

#endif
