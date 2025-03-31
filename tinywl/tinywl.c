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
