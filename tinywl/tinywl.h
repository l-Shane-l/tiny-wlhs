#ifndef TINYWL_H
#define TINYWL_H

#ifndef WLR_USE_UNSTABLE
#define WLR_USE_UNSTABLE
#endif

#include "wlr-layer-shell-unstable-v1-protocol.h"
#include <stdbool.h>
#include <stdint.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_layer_shell_v1.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <xkbcommon/xkbcommon.h>

#include <assert.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <wayland-server-core.h>
#include <wayland-server-protocol.h>
#include <wlr/backend.h>
#include <wlr/backend/interface.h>
#include <wlr/backend/session.h>
#include <wlr/render/allocator.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_subcompositor.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_xdg_activation_v1.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>

typedef void (*keybinding_handler_t)(xkb_keysym_t sym);

static uint32_t global_modifier = WLR_MODIFIER_LOGO; // Default

void set_modifier_key(uint32_t modifier) { global_modifier = modifier; }

// Forward declarations for opaque types
struct wlr_backend;
struct wlr_renderer;
struct wlr_allocator;
struct wlr_scene;
struct wlr_scene_output_layout;
struct wlr_cursor;
struct wlr_xcursor_manager;
struct wlr_seat;
struct wlr_box;

enum tinywl_cursor_mode {
  TINYWL_CURSOR_PASSTHROUGH,
  TINYWL_CURSOR_MOVE,
  TINYWL_CURSOR_RESIZE,
};

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
};

struct tinywl_toplevel_decoration {
  struct wl_list link;
  struct tinywl_server *server;
  struct wlr_xdg_toplevel_decoration_v1 *wlr_decoration;
  struct wl_listener destroy;
  struct wl_listener request_mode;
};

struct reserved_area {
  int x, y;
  int width, height;
};

struct output_layout_data {
  struct reserved_area reserved;
  struct wlr_box usable_area;
};

// Add to tinywl_server struct:

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

struct tinywl_output {
  struct wl_list link;
  struct tinywl_server *server;
  struct wlr_output *wlr_output;
  struct wl_listener frame;
  struct wl_listener request_state;
  struct wl_listener destroy;
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

struct tinywl_keyboard {
  struct wl_list link;
  struct tinywl_server *server;
  struct wlr_keyboard *wlr_keyboard;
  struct wl_listener modifiers;
  struct wl_listener key;
  struct wl_listener destroy;
};

// Server lifecycle functions
struct tinywl_server *server_create(void);
void server_destroy(struct tinywl_server *server);
bool server_init(struct tinywl_server *server);
const char *server_start(struct tinywl_server *server);
static void set_border_color(struct tinywl_toplevel *toplevel, bool focused);
static void update_border_position(struct tinywl_toplevel *toplevel);
static void focus_layer_surface(struct tinywl_server *server,
                                struct wlr_surface *surface);
void server_run(struct tinywl_server *server);
void server_set_startup_command(const char *cmd);
bool cycle_windows(struct tinywl_server *server);
static void handle_xdg_activation_v1_request(struct wl_listener *listener,
                                             void *data);

bool initialize_backend_renderer_allocator(struct tinywl_server *server);

// Key handling functions
void set_haskell_key_notification_function(void (*func)(xkb_keysym_t sym));

#endif // TINYWL_H
