#ifndef TOPLEVEL_H
#define TOPLEVEL_H

#include "surface.h"
#include "wayland-server-core.h"
#include "wayland-util.h"

struct tinywl_toplevel_decoration {
  struct wl_list link;
  struct tinywl_server *server;
  struct wlr_xdg_toplevel_decoration_v1 *wlr_decoration;
  struct wl_listener destroy;
  struct wl_listener request_mode;
};

struct tinywl_toplevel {
  struct wl_list link;
  struct tinywl_server *server;
  struct wlr_xdg_toplevel *xdg_toplevel;
  struct wlr_scene_tree *scene_tree;

  struct wlr_scene_tree *border_tree;

  // Border rectangles
  struct wlr_scene_rect *border_top;
  struct wlr_scene_rect *border_bottom;
  struct wlr_scene_rect *border_left;
  struct wlr_scene_rect *border_right;

  struct wl_listener commit;

  struct wl_listener map;
  struct wl_listener unmap;
  struct wl_listener destroy;
  struct wl_listener request_move;
  struct wl_listener request_resize;
  struct wl_listener request_maximize;
  struct wl_listener request_fullscreen;
};

void focus_toplevel(struct tinywl_toplevel *toplevel,
                    struct wlr_surface *surface);

void set_border_color(struct tinywl_toplevel *toplevel, bool focused);
void update_border_position(struct tinywl_toplevel *toplevel);
void xdg_toplevel_request_move(struct wl_listener *listener, void *data);
void xdg_toplevel_request_resize(struct wl_listener *listener, void *data);
void xdg_toplevel_request_maximize(struct wl_listener *listener, void *data);
void xdg_toplevel_request_fullscreen(struct wl_listener *listener, void *data);
void xdg_toplevel_map(struct wl_listener *listener, void *data);
void xdg_toplevel_unmap(struct wl_listener *listener, void *data);
void xdg_toplevel_destroy(struct wl_listener *listener, void *data);
void handle_toplevel_commit(struct wl_listener *listener, void *data);
#endif
