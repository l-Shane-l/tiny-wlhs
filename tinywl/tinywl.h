#ifndef TINYWL_H
#define TINYWL_H

#ifndef WLR_USE_UNSTABLE
#define WLR_USE_UNSTABLE
#endif

#include "wlr-layer-shell-unstable-v1-protocol.h"
#include <stdbool.h>
#include <stdint.h>
#include <wayland-server-core.h>

#include "include/cursor.h"
#include "include/input.h"
#include "include/layer.h"
#include "include/output.h"
#include "include/server.h"
#include "include/toplevel.h"

#include "include/surface.h"
#include "include/toplevel.h"
#include <wlr/types/wlr_layer_shell_v1.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <xkbcommon/xkbcommon.h>

#include <assert.h>
#include <getopt.h>
#include <stdbool.h>

#include <unistd.h>
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
#include <wlr/types/wlr_primary_selection_v1.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_subcompositor.h>

#include <wlr/types/wlr_xdg_activation_v1.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>

struct reserved_area {
  int x, y;
  int width, height;
};

struct output_layout_data {
  struct reserved_area reserved;
  struct wlr_box usable_area;
};

// Add to tinywl_server struct:

// Server lifecycle functions
struct tinywl_server *server_create(void);
void server_destroy(struct tinywl_server *server);
bool server_init(struct tinywl_server *server);
const char *server_start(struct tinywl_server *server);

void server_run(struct tinywl_server *server);
void server_set_startup_command(const char *cmd);
bool cycle_windows(struct tinywl_server *server);
static void handle_xdg_activation_v1_request(struct wl_listener *listener,
                                             void *data);

bool initialize_backend_renderer_allocator(struct tinywl_server *server);

// Key handling functions
void set_haskell_key_notification_function(void (*func)(xkb_keysym_t sym));
void begin_interactive(struct tinywl_toplevel *toplevel,
                       enum tinywl_cursor_mode mode, uint32_t edges);

#endif // TINYWL_H
