#define _POSIX_C_SOURCE 200112L
#include "tinywl.h"
#include <limits.h>
#include <stdlib.h>
#include <time.h>

bool cycle_windows(struct tinywl_server *server) {
  if (wl_list_length(&server->toplevels) < 2) {
    return false;
  }

  // Find the currently focused window
  struct wlr_surface *focused_surface =
      server->seat->keyboard_state.focused_surface;
  struct tinywl_toplevel *current = NULL;
  struct tinywl_toplevel *first = NULL;
  struct tinywl_toplevel *next = NULL;

  // Find the current and next window to focus
  struct tinywl_toplevel *toplevel;
  wl_list_for_each(toplevel, &server->toplevels, link) {
    if (!first) {
      first = toplevel;
    }

    if (current && !next) {
      next = toplevel;
      break;
    }

    if (toplevel->xdg_toplevel->base->surface == focused_surface) {
      current = toplevel;
    }
  }

  // If we didn't find a next window, wrap around to the first
  if (!next) {
    next = first;
  }

  // If we somehow didn't find a window to focus, return false
  if (!next) {
    return false;
  }

  // Focus the next window
  focus_toplevel(next, next->xdg_toplevel->base->surface);
  return true;
}

static void server_new_pointer(struct tinywl_server *server,
                               struct wlr_input_device *device) {
  /* We don't do anything special with pointers. All of our pointer handling
   * is proxied through wlr_cursor. On another compositor, you might take this
   * opportunity to do libinput configuration on the device to set
   * acceleration, etc. */
  wlr_cursor_attach_input_device(server->cursor, device);
}

static void server_new_input(struct wl_listener *listener, void *data) {
  /* This event is raised by the backend when a new input device becomes
   * available. */
  struct tinywl_server *server = wl_container_of(listener, server, new_input);
  struct wlr_input_device *device = data;
  switch (device->type) {
  case WLR_INPUT_DEVICE_KEYBOARD:
    server_new_keyboard(server, device);
    break;
  case WLR_INPUT_DEVICE_POINTER:
    server_new_pointer(server, device);
    break;
  default:
    break;
  }
  /* We need to let the wlr_seat know what our capabilities are, which is
   * communiciated to the client. In TinyWL we always have a cursor, even if
   * there are no pointer devices, so we always include that capability. */
  uint32_t caps = WL_SEAT_CAPABILITY_POINTER;
  if (!wl_list_empty(&server->keyboards)) {
    caps |= WL_SEAT_CAPABILITY_KEYBOARD;
  }
  wlr_seat_set_capabilities(server->seat, caps);
}

static void seat_request_cursor(struct wl_listener *listener, void *data) {
  struct tinywl_server *server =
      wl_container_of(listener, server, request_cursor);
  /* This event is raised by the seat when a client provides a cursor image */
  struct wlr_seat_pointer_request_set_cursor_event *event = data;
  struct wlr_seat_client *focused_client =
      server->seat->pointer_state.focused_client;
  /* This can be sent by any client, so we check to make sure this one is
   * actually has pointer focus first. */
  if (focused_client == event->seat_client) {
    /* Once we've vetted the client, we can tell the cursor to use the
     * provided surface as the cursor image. It will set the hardware cursor
     * on the output that it's currently on and continue to do so as the
     * cursor moves between outputs. */
    wlr_cursor_set_surface(server->cursor, event->surface, event->hotspot_x,
                           event->hotspot_y);
  }
}

static void seat_request_set_selection(struct wl_listener *listener,
                                       void *data) {
  /* This event is raised by the seat when a client wants to set the selection,
   * usually when the user copies something. wlroots allows compositors to
   * ignore such requests if they so choose, but in tinywl we always honor
   */
  struct tinywl_server *server =
      wl_container_of(listener, server, request_set_selection);
  struct wlr_seat_request_set_selection_event *event = data;
  wlr_seat_set_selection(server->seat, event->source, event->serial);
}

void reset_cursor_mode(struct tinywl_server *server) {
  /* Reset the cursor mode to passthrough. */
  server->cursor_mode = TINYWL_CURSOR_PASSTHROUGH;
  server->grabbed_toplevel = NULL;
}

static void process_cursor_move(struct tinywl_server *server, uint32_t time) {
  /* Move the grabbed toplevel to the new position. */
  struct tinywl_toplevel *toplevel = server->grabbed_toplevel;
  wlr_scene_node_set_position(&toplevel->scene_tree->node,
                              server->cursor->x - server->grab_x,
                              server->cursor->y - server->grab_y);
}

static void process_cursor_resize(struct tinywl_server *server, uint32_t time) {
  struct tinywl_toplevel *toplevel = server->grabbed_toplevel;
  const int border_thickness = 5;

  double border_x = server->cursor->x - server->grab_x;
  double border_y = server->cursor->y - server->grab_y;
  int new_left = server->grab_geobox.x;
  int new_right = server->grab_geobox.x + server->grab_geobox.width;
  int new_top = server->grab_geobox.y;
  int new_bottom = server->grab_geobox.y + server->grab_geobox.height;

  if (server->resize_edges & WLR_EDGE_TOP) {
    new_top = border_y;
    if (new_top >= new_bottom - 2 * border_thickness) {
      new_top = new_bottom - 2 * border_thickness - 1;
    }
  } else if (server->resize_edges & WLR_EDGE_BOTTOM) {
    new_bottom = border_y;
    if (new_bottom <= new_top + 2 * border_thickness) {
      new_bottom = new_top + 2 * border_thickness + 1;
    }
  }
  if (server->resize_edges & WLR_EDGE_LEFT) {
    new_left = border_x;
    if (new_left >= new_right - 2 * border_thickness) {
      new_left = new_right - 2 * border_thickness - 1;
    }
  } else if (server->resize_edges & WLR_EDGE_RIGHT) {
    new_right = border_x;
    if (new_right <= new_left + 2 * border_thickness) {
      new_right = new_left + 2 * border_thickness + 1;
    }
  }

  struct wlr_box geo_box;
  wlr_xdg_surface_get_geometry(toplevel->xdg_toplevel->base, &geo_box);

  wlr_scene_node_set_position(&toplevel->scene_tree->node, new_left - geo_box.x,
                              new_top - geo_box.y);

  // Calculate new dimensions accounting for borders
  int new_width = new_right - new_left - (2 * border_thickness);
  int new_height = new_bottom - new_top - (2 * border_thickness);

  // Ensure minimum size
  if (new_width < 1)
    new_width = 1;
  if (new_height < 1)
    new_height = 1;

  wlr_xdg_toplevel_set_size(toplevel->xdg_toplevel, new_width, new_height);
  update_border_position(toplevel);
}

static void process_cursor_motion(struct tinywl_server *server, uint32_t time) {
  /* If the mode is non-passthrough, delegate to those functions. */
  if (server->cursor_mode == TINYWL_CURSOR_MOVE) {
    process_cursor_move(server, time);
    return;
  } else if (server->cursor_mode == TINYWL_CURSOR_RESIZE) {
    process_cursor_resize(server, time);
    return;
  }

  /* First check for layer surfaces */
  double sx, sy;
  struct wlr_seat *seat = server->seat;
  struct wlr_surface *surface = NULL;

  /* Check layer surfaces first */
  struct wlr_scene_node *node =
      wlr_scene_node_at(&server->scene->tree.node, server->cursor->x,
                        server->cursor->y, &sx, &sy);
  if (node && node->type == WLR_SCENE_NODE_BUFFER) {
    struct wlr_scene_buffer *scene_buffer = wlr_scene_buffer_from_node(node);
    struct wlr_scene_surface *scene_surface =
        wlr_scene_surface_try_from_buffer(scene_buffer);
    if (scene_surface) {
      surface = scene_surface->surface;
    }
  }

  /* If no layer surface found, check for regular toplevels */
  if (!surface) {
    struct tinywl_toplevel *toplevel = desktop_toplevel_at(
        server, server->cursor->x, server->cursor->y, &surface, &sx, &sy);

    if (!toplevel) {
      /* If there's no toplevel, set the default cursor */
      wlr_cursor_set_xcursor(server->cursor, server->cursor_mgr, "default");
    }
  }

  if (surface) {
    wlr_seat_pointer_notify_enter(seat, surface, sx, sy);
    wlr_seat_pointer_notify_motion(seat, time, sx, sy);
  } else {
    wlr_seat_pointer_clear_focus(seat);
  }
}

static void server_cursor_motion(struct wl_listener *listener, void *data) {
  /* This event is forwarded by the cursor when a pointer emits a _relative_
   * pointer motion event (i.e. a delta) */
  struct tinywl_server *server =
      wl_container_of(listener, server, cursor_motion);
  struct wlr_pointer_motion_event *event = data;
  /* The cursor doesn't move unless we tell it to. The cursor automatically
   * handles constraining the motion to the output layout, as well as any
   * special configuration applied for the specific input device which
   * generated the event. You can pass NULL for the device if you want to move
   * the cursor around without any input. */
  wlr_cursor_move(server->cursor, &event->pointer->base, event->delta_x,
                  event->delta_y);
  process_cursor_motion(server, event->time_msec);
}

static void server_cursor_motion_absolute(struct wl_listener *listener,
                                          void *data) {
  /* This event is forwarded by the cursor when a pointer emits an _absolute_
   * motion event, from 0..1 on each axis. This happens, for example, when
   * wlroots is running under a Wayland window rather than KMS+DRM, and you
   * move the mouse over the window. You could enter the window from any edge,
   * so we have to warp the mouse there. There is also some hardware which
   * emits these events. */
  struct tinywl_server *server =
      wl_container_of(listener, server, cursor_motion_absolute);
  struct wlr_pointer_motion_absolute_event *event = data;
  wlr_cursor_warp_absolute(server->cursor, &event->pointer->base, event->x,
                           event->y);
  process_cursor_motion(server, event->time_msec);
}

static void server_cursor_button(struct wl_listener *listener, void *data) {
  struct tinywl_server *server =
      wl_container_of(listener, server, cursor_button);
  struct wlr_pointer_button_event *event = data;

  // Check for layer surface first
  double sx, sy;
  struct wlr_surface *surface =
      layer_surface_at(server, server->cursor->x, server->cursor->y, &sx, &sy);

  if (surface) {
    // If we found a layer surface, focus it and forward the click
    focus_layer_surface(server, surface);
    wlr_seat_pointer_notify_button(server->seat, event->time_msec,
                                   event->button, event->state);
    return;
  }

  // If no layer surface, check for regular windows
  surface = NULL;
  struct tinywl_toplevel *toplevel = desktop_toplevel_at(
      server, server->cursor->x, server->cursor->y, &surface, &sx, &sy);

  wlr_seat_pointer_notify_button(server->seat, event->time_msec, event->button,
                                 event->state);

  if (event->state == WLR_BUTTON_RELEASED) {
    reset_cursor_mode(server);
  } else {
    focus_toplevel(toplevel, surface);
  }
}

static void server_cursor_axis(struct wl_listener *listener, void *data) {
  /* This event is forwarded by the cursor when a pointer emits an axis event,
   * for example when you move the scroll wheel. */
  struct tinywl_server *server = wl_container_of(listener, server, cursor_axis);
  struct wlr_pointer_axis_event *event = data;
  /* Notify the client with pointer focus of the axis event. */
  wlr_seat_pointer_notify_axis(server->seat, event->time_msec,
                               event->orientation, event->delta,
                               event->delta_discrete, event->source);
}

static void server_cursor_frame(struct wl_listener *listener, void *data) {
  /* This event is forwarded by the cursor when a pointer emits an frame
   * event. Frame events are sent after regular pointer events to group
   * multiple events together. For instance, two axis events may happen at the
   * same time, in which case a frame event won't be sent in between. */
  struct tinywl_server *server =
      wl_container_of(listener, server, cursor_frame);
  /* Notify the client with pointer focus of the frame event. */
  wlr_seat_pointer_notify_frame(server->seat);
}

static void output_frame(struct wl_listener *listener, void *data) {
  struct tinywl_output *output = wl_container_of(listener, output, frame);
  struct wlr_scene *scene = output->server->scene;

  struct wlr_scene_output *scene_output =
      wlr_scene_get_scene_output(scene, output->wlr_output);
  if (!scene_output) {
    wlr_log(WLR_ERROR, "No scene output!");
    return;
  }

  // wlr_log(WLR_DEBUG, "Rendering frame for output %s",
  // output->wlr_output->name);

  /* Render the scene */
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);

  if (!wlr_scene_output_commit(scene_output, NULL)) {
    wlr_log(WLR_ERROR, "Failed to commit scene output!");
    return;
  }

  wlr_scene_output_send_frame_done(scene_output, &now);
}

static void output_request_state(struct wl_listener *listener, void *data) {
  /* This function is called when the backend requests a new state for
   * the output. For example, Wayland and X11 backends request a new mode
   * when the output window is resized. */
  struct tinywl_output *output =
      wl_container_of(listener, output, request_state);
  const struct wlr_output_event_request_state *event = data;
  wlr_output_commit_state(output->wlr_output, event->state);
}

static void output_destroy(struct wl_listener *listener, void *data) {
  struct tinywl_output *output = wl_container_of(listener, output, destroy);

  wl_list_remove(&output->frame.link);
  wl_list_remove(&output->request_state.link);
  wl_list_remove(&output->destroy.link);
  wl_list_remove(&output->link);
  free(output);
}

void set_modifier_key(uint32_t modifier) { global_modifier = modifier; }

static void server_new_output(struct wl_listener *listener, void *data) {
  struct tinywl_server *server = wl_container_of(listener, server, new_output);
  struct wlr_output *wlr_output = data;

  wlr_log(WLR_DEBUG, "New output: %s", wlr_output->name);
  wlr_log(WLR_DEBUG, "Output formats:");

  size_t formats_len;

  /* Configures the output created by the backend to use our allocator
   * and our renderer. Must be done once, before commiting the output */
  wlr_output_init_render(wlr_output, server->allocator, server->renderer);

  /* The output may be disabled, switch it on. */
  struct wlr_output_state state;
  wlr_output_state_init(&state);
  wlr_output_state_set_enabled(&state, true);

  /* Some backends don't have modes. DRM+KMS does, and we need to set a mode
   * before we can use the output. The mode is a tuple of (width, height,
   * refresh rate), and each monitor supports only a specific set of modes. We
   * just pick the monitor's preferred mode, a more sophisticated compositor
   * would let the user configure it. */
  struct wlr_output_mode *mode = wlr_output_preferred_mode(wlr_output);
  if (mode != NULL) {
    wlr_output_state_set_mode(&state, mode);
  }

  /* Atomically applies the new output state. */
  wlr_output_commit_state(wlr_output, &state);
  wlr_output_state_finish(&state);

  /* Allocates and configures our state for this output */
  struct tinywl_output *output = calloc(1, sizeof(*output));
  output->wlr_output = wlr_output;
  output->server = server;

  /* Sets up a listener for the frame event. */
  output->frame.notify = output_frame;
  wl_signal_add(&wlr_output->events.frame, &output->frame);

  /* Sets up a listener for the state request event. */
  output->request_state.notify = output_request_state;
  wl_signal_add(&wlr_output->events.request_state, &output->request_state);

  /* Sets up a listener for the destroy event. */
  output->destroy.notify = output_destroy;
  wl_signal_add(&wlr_output->events.destroy, &output->destroy);

  wl_list_insert(&server->outputs, &output->link);

  /* Adds this to the output layout. The add_auto function arranges outputs
   * from left-to-right in the order they appear. A more sophisticated
   * compositor would let the user configure the arrangement of outputs in the
   * layout.
   *
   * The output layout utility automatically adds a wl_output global to the
   * display, which Wayland clients can see to find out information about the
   * output (such as DPI, scale factor, manufacturer, etc).
   */
  struct wlr_output_layout_output *l_output =
      wlr_output_layout_add_auto(server->output_layout, wlr_output);
  struct wlr_scene_output *scene_output =
      wlr_scene_output_create(server->scene, wlr_output);
  wlr_scene_output_layout_add_output(server->scene_layout, l_output,
                                     scene_output);
}

void begin_interactive(struct tinywl_toplevel *toplevel,
                       enum tinywl_cursor_mode mode, uint32_t edges) {
  /* This function sets up an interactive move or resize operation, where the
   * compositor stops propegating pointer events to clients and instead
   * consumes them itself, to move or resize windows. */
  struct tinywl_server *server = toplevel->server;
  struct wlr_surface *focused_surface =
      server->seat->pointer_state.focused_surface;
  if (toplevel->xdg_toplevel->base->surface !=
      wlr_surface_get_root_surface(focused_surface)) {
    /* Deny move/resize requests from unfocused clients. */
    return;
  }
  server->grabbed_toplevel = toplevel;
  server->cursor_mode = mode;

  if (mode == TINYWL_CURSOR_MOVE) {
    server->grab_x = server->cursor->x - toplevel->scene_tree->node.x;
    server->grab_y = server->cursor->y - toplevel->scene_tree->node.y;
  } else {
    struct wlr_box geo_box;
    wlr_xdg_surface_get_geometry(toplevel->xdg_toplevel->base, &geo_box);

    double border_x = (toplevel->scene_tree->node.x + geo_box.x) +
                      ((edges & WLR_EDGE_RIGHT) ? geo_box.width : 0);
    double border_y = (toplevel->scene_tree->node.y + geo_box.y) +
                      ((edges & WLR_EDGE_BOTTOM) ? geo_box.height : 0);
    server->grab_x = server->cursor->x - border_x;
    server->grab_y = server->cursor->y - border_y;

    server->grab_geobox = geo_box;
    server->grab_geobox.x += toplevel->scene_tree->node.x;
    server->grab_geobox.y += toplevel->scene_tree->node.y;

    server->resize_edges = edges;
  }
}

// Function declarations
char *parse_arguments(int argc, char *argv[]);
void initialize_output_layout(struct tinywl_server *server);
void initialize_scene(struct tinywl_server *server);
void initialize_xdg_shell(struct tinywl_server *server);
void initialize_cursor(struct tinywl_server *server);
void initialize_seat(struct tinywl_server *server);
const char *start_backend(struct tinywl_server *server);
void run_startup_command(const char *startup_cmd);
void cleanup(struct tinywl_server *server);
struct tinywl_server *server_create();
void server_destroy(struct tinywl_server *server);
bool server_init(struct tinywl_server *server);
const char *server_start(struct tinywl_server *server);
void server_run(struct tinywl_server *server);
void server_set_startup_command(const char *cmd);

// New server functions
struct tinywl_server *server_create() {
  struct tinywl_server *server = calloc(1, sizeof(struct tinywl_server));
  return server;
}

void server_destroy(struct tinywl_server *server) {
  cleanup(server);
  free(server);
}

static void initialize_layer_trees(struct tinywl_server *server) {
  // Create all the layer scene trees
  server->layer_tree_background = wlr_scene_tree_create(&server->scene->tree);
  server->layer_tree_bottom = wlr_scene_tree_create(&server->scene->tree);
  server->xdg_shell_tree = wlr_scene_tree_create(&server->scene->tree);
  server->layer_tree_top = wlr_scene_tree_create(&server->scene->tree);
  server->layer_tree_overlay = wlr_scene_tree_create(&server->scene->tree);

  // Ensure each node is valid
  assert(server->layer_tree_background != NULL);
  assert(server->layer_tree_bottom != NULL);
  assert(server->xdg_shell_tree != NULL);
  assert(server->layer_tree_top != NULL);
  assert(server->layer_tree_overlay != NULL);

  // Stack them strictly in the correct order
  struct wlr_scene_node *last_node = &server->layer_tree_background->node;
  wlr_scene_node_lower_to_bottom(last_node);

  last_node = &server->layer_tree_bottom->node;
  wlr_scene_node_place_above(last_node, &server->layer_tree_background->node);

  last_node = &server->xdg_shell_tree->node;
  wlr_scene_node_place_above(last_node, &server->layer_tree_bottom->node);

  last_node = &server->layer_tree_top->node;
  wlr_scene_node_place_above(last_node, &server->xdg_shell_tree->node);

  last_node = &server->layer_tree_overlay->node;
  wlr_scene_node_place_above(last_node, &server->layer_tree_top->node);
}

void initialize_layer_shell(struct tinywl_server *server) {
  wl_list_init(&server->layer_surfaces);
  server->layer_shell = wlr_layer_shell_v1_create(server->wl_display, 3);
  if (!server->layer_shell) {
    wlr_log(WLR_ERROR, "Failed to create layer shell");
    return;
  }
  server->new_layer_surface.notify = server_new_layer_surface;
  wl_signal_add(&server->layer_shell->events.new_surface,
                &server->new_layer_surface);
  wlr_log(WLR_DEBUG, "Layer shell initialized successfully");
}

bool server_init(struct tinywl_server *server) {
  wlr_log_init(WLR_DEBUG, NULL);
  // setenv("WAYLAND_DEBUG", "1", 1); // this gives really verbose logs on
  // wayland protocols

  // Basic display/backend setup first
  if (!initialize_backend_renderer_allocator(server)) {
    return false;
  }

  // The setup is currently diffived between here and
  // A similar function implemented in haskell
  // Ultimately, this code here will be moved to haskell
  // , however its convient to have it here while there
  // is still a lot of protocol support to be added and this
  // is always done initialally in C

  printf("server setup phase 1 complete");

  // Protocol support now second
  server->xdg_activation = wlr_xdg_activation_v1_create(server->wl_display);
  server->new_activation_request.notify = handle_xdg_activation_v1_request;
  wl_signal_add(&server->xdg_activation->events.request_activate,
                &server->new_activation_request);
  initialize_xdg_shell(server);
  initialize_layer_shell(server);

  // Initialize clipboard support
  server->data_device_manager =
      wlr_data_device_manager_create(server->wl_display);
  if (!server->data_device_manager) {
    wlr_log(WLR_ERROR, "Unable to create data device manager");
    return false;
  }

  // Output and scene setup
  initialize_output_layout(server);
  initialize_scene(server);

  // Input handling setup (seat and cursor)
  initialize_seat(server);
  initialize_cursor(server);

  initialize_layer_trees(server);
  wl_list_init(&server->decorations);
  server->xdg_decoration_manager =
      wlr_xdg_decoration_manager_v1_create(server->wl_display);
  if (!server->xdg_decoration_manager) {
    wlr_log(WLR_ERROR, "Unable to create XDG decoration manager");
    return false;
  }

  server->new_xdg_decoration.notify = server_handle_new_xdg_decoration;
  wl_signal_add(&server->xdg_decoration_manager->events.new_toplevel_decoration,
                &server->new_xdg_decoration);

  return true;
}

static void handle_xdg_activation_v1_request(struct wl_listener *listener,
                                             void *data) {
  struct tinywl_server *server =
      wl_container_of(listener, server, new_activation_request);
  struct wlr_xdg_activation_v1_request_activate_event *event = data;

  /* Skip if surface is already focused */
  struct wlr_surface *surface = event->surface;
  if (surface == server->seat->keyboard_state.focused_surface) {
    return;
  }

  /* Find the toplevel and focus it */
  struct wlr_xdg_surface *xdg_surface =
      wlr_xdg_surface_try_from_wlr_surface(surface);
  if (xdg_surface != NULL &&
      xdg_surface->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL) {
    struct tinywl_toplevel *toplevel = xdg_surface->data;
    if (toplevel != NULL) {
      focus_toplevel(toplevel, surface);
    }
  }
}

const char *server_start(struct tinywl_server *server) {
  return start_backend(server);
}

void server_run(struct tinywl_server *server) {
  wl_display_run(server->wl_display);
}

void server_set_startup_command(const char *cmd) { run_startup_command(cmd); }

char *parse_arguments(int argc, char *argv[]) {
  char *startup_cmd = NULL;
  int c;
  while ((c = getopt(argc, argv, "s:h")) != -1) {
    switch (c) {
    case 's':
      startup_cmd = optarg;
      break;
    default:
      printf("Usage: %s [-s startup command]\n", argv[0]);
      return NULL;
    }
  }
  if (optind < argc) {
    printf("Usage: %s [-s startup command]\n", argv[0]);
    return NULL;
  }
  return startup_cmd;
}

void initialize_output_layout(struct tinywl_server *server) {
  server->output_layout = wlr_output_layout_create();
  wl_list_init(&server->outputs);
  server->new_output.notify = server_new_output;
  wl_signal_add(&server->backend->events.new_output, &server->new_output);
}

void initialize_scene(struct tinywl_server *server) {
  server->scene = wlr_scene_create();
  server->scene_layout =
      wlr_scene_attach_output_layout(server->scene, server->output_layout);
}

void initialize_xdg_shell(struct tinywl_server *server) {
  wl_list_init(&server->toplevels);
  server->xdg_shell = wlr_xdg_shell_create(server->wl_display, 3);
  server->new_xdg_surface.notify = server_new_xdg_surface;
  wl_signal_add(&server->xdg_shell->events.new_surface,
                &server->new_xdg_surface);
}

void initialize_cursor(struct tinywl_server *server) {
  server->cursor = wlr_cursor_create();
  wlr_cursor_attach_output_layout(server->cursor, server->output_layout);
  server->cursor_mgr = wlr_xcursor_manager_create(NULL, 24);
  server->cursor_mode = TINYWL_CURSOR_PASSTHROUGH;
  server->cursor_motion.notify = server_cursor_motion;
  wl_signal_add(&server->cursor->events.motion, &server->cursor_motion);
  server->cursor_motion_absolute.notify = server_cursor_motion_absolute;
  wl_signal_add(&server->cursor->events.motion_absolute,
                &server->cursor_motion_absolute);
  server->cursor_button.notify = server_cursor_button;
  wl_signal_add(&server->cursor->events.button, &server->cursor_button);
  server->cursor_axis.notify = server_cursor_axis;
  wl_signal_add(&server->cursor->events.axis, &server->cursor_axis);
  server->cursor_frame.notify = server_cursor_frame;
  wl_signal_add(&server->cursor->events.frame, &server->cursor_frame);
}

void initialize_seat(struct tinywl_server *server) {
  wl_list_init(&server->keyboards);
  server->new_input.notify = server_new_input;
  wl_signal_add(&server->backend->events.new_input, &server->new_input);
  server->seat = wlr_seat_create(server->wl_display, "seat0");
  server->request_cursor.notify = seat_request_cursor;
  wl_signal_add(&server->seat->events.request_set_cursor,
                &server->request_cursor);
  server->request_set_selection.notify = seat_request_set_selection;
  wl_signal_add(&server->seat->events.request_set_selection,
                &server->request_set_selection);
}

bool initialize_backend_renderer_allocator(struct tinywl_server *server) {
  wlr_log(WLR_ERROR, "initializing backe renderer allocator");

  if (!wlr_renderer_init_wl_display(server->renderer, server->wl_display)) {
    wlr_log(WLR_ERROR, "Failed to initialize renderer with display");
    return false;
  }

  /* Create compositor and necessary interfaces */
  struct wlr_compositor *compositor =
      wlr_compositor_create(server->wl_display, 5, server->renderer);
  if (!compositor) {
    wlr_log(WLR_ERROR, "Failed to create compositor");
    return false;
  }

  if (!wlr_subcompositor_create(server->wl_display)) {
    wlr_log(WLR_ERROR, "Failed to create subcompositor");
    return false;
  }

  if (!wlr_data_device_manager_create(server->wl_display)) {
    wlr_log(WLR_ERROR, "Failed to create data device manager");
    return false;
  }

  return true;
}

const char *start_backend(struct tinywl_server *server) {
  const char *socket = wl_display_add_socket_auto(server->wl_display);
  if (!socket) {
    wlr_backend_destroy(server->backend);
    return NULL;
  }

  if (!wlr_backend_start(server->backend)) {
    wlr_backend_destroy(server->backend);
    wl_display_destroy(server->wl_display);
    return NULL;
  }

  return socket;
}

void run_startup_command(const char *startup_cmd) {
  if (startup_cmd) {
    if (fork() == 0) {
      execl("/bin/sh", "/bin/sh", "-c", startup_cmd, (void *)NULL);
    }
  }
}

void cleanup(struct tinywl_server *server) {
  wl_display_destroy_clients(server->wl_display);
  wlr_scene_node_destroy(&server->scene->tree.node);
  wlr_xcursor_manager_destroy(server->cursor_mgr);
  wlr_output_layout_destroy(server->output_layout);
  wl_display_destroy(server->wl_display);
}
