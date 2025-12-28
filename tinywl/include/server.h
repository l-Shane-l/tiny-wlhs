#ifndef SERVER_H
#define SERVER_H

#include "input.h"
#include "wayland-server-core.h"
#include "wlr/util/box.h"
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_xcursor_manager.h>

enum tinywl_cursor_mode {
  TINYWL_CURSOR_PASSTHROUGH,
  TINYWL_CURSOR_MOVE,
  TINYWL_CURSOR_RESIZE,
};

struct wlr_backend;
struct wlr_renderer;
struct wlr_allocator;
struct wlr_scene;
struct wlr_scene_output_layout;

struct wlr_xcursor_manager;
struct wlr_seat;
struct wlr_box;
struct wlr_cursor;

struct tinywl_server {
  struct wl_display *wl_display;
  struct wlr_backend *backend;
  struct wlr_renderer *renderer;
  struct wlr_allocator *allocator;
  struct wlr_scene *scene;
  struct wlr_scene_output_layout *scene_layout;

  struct wlr_xdg_shell *xdg_shell;
  struct wl_listener new_xdg_surface;
  struct wl_list toplevels;

  struct wlr_cursor *cursor;
  struct wlr_xcursor_manager *cursor_mgr;
  struct wl_listener cursor_motion;
  struct wl_listener cursor_motion_absolute;
  struct wl_listener cursor_button;
  struct wl_listener cursor_axis;
  struct wl_listener cursor_frame;

  struct wlr_seat *seat;
  struct wl_listener new_input;
  struct wl_listener request_cursor;
  struct wl_listener request_set_selection;
  struct wl_list keyboards;
  enum tinywl_cursor_mode cursor_mode;
  struct tinywl_toplevel *grabbed_toplevel;
  double grab_x, grab_y;
  struct wlr_box grab_geobox;
  uint32_t resize_edges;

  struct wlr_output_layout *output_layout;
  struct wl_list outputs;
  struct wl_listener new_output;

  keybinding_handler_t keybinding_handler;
  struct wlr_layer_shell_v1 *layer_shell;
  struct wl_listener new_layer_surface;
  struct wl_list layer_surfaces;
  struct wl_listener configure;

  struct wlr_xdg_activation_v1 *xdg_activation;
  struct wl_listener new_activation_request;

  struct wlr_session *session;

  struct wlr_scene_tree *layer_tree_background;
  struct wlr_scene_tree *layer_tree_bottom;
  struct wlr_scene_tree *layer_tree_top;
  struct wlr_scene_tree *layer_tree_overlay;

  struct wl_list decorations; // tinywl_toplevel_decoration::link
  struct wlr_xdg_decoration_manager_v1 *xdg_decoration_manager;
  struct wl_listener new_xdg_decoration;

  struct wlr_scene_tree *xdg_shell_tree;
  struct tinywl_workspace *workspaces; // Array of workspaces
  int active_workspace;

  struct wlr_primary_selection_v1_device_manager *primary_selection_manager;
  struct wl_listener request_set_primary_selection;
  struct wlr_data_device_manager *data_device_manager;
};

void server_new_pointer(struct tinywl_server *server,
                        struct wlr_input_device *device);

void server_new_input(struct wl_listener *listener, void *data);
void process_cursor_move(struct tinywl_server *server, uint32_t time);
void process_cursor_motion(struct tinywl_server *server, uint32_t time);
void server_cursor_motion(struct wl_listener *listener, void *data);
void server_cursor_motion_absolute(struct wl_listener *listener, void *data);
void server_cursor_button(struct wl_listener *listener, void *data);
void server_cursor_axis(struct wl_listener *listener, void *data);
void server_cursor_frame(struct wl_listener *listener, void *data);
#endif
