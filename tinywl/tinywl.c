#define _POSIX_C_SOURCE 200112L
#include "tinywl.h"
#include <limits.h>

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

void focus_toplevel(struct tinywl_toplevel *toplevel,
                    struct wlr_surface *surface) {
  /* Note: this function only deals with keyboard focus. */
  if (toplevel == NULL) {
    return;
  }
  struct tinywl_server *server = toplevel->server;
  struct wlr_seat *seat = server->seat;
  struct wlr_surface *prev_surface = seat->keyboard_state.focused_surface;
  if (prev_surface == surface) {
    /* Don't re-focus an already focused surface. */
    return;
  }
  if (prev_surface) {
    /*
     * Deactivate the previously focused surface. This lets the client know
     * it no longer has focus and the client will repaint accordingly, e.g.
     * stop displaying a caret.
     */
    struct wlr_xdg_toplevel *prev_toplevel =
        wlr_xdg_toplevel_try_from_wlr_surface(prev_surface);
    if (prev_toplevel != NULL) {
      wlr_xdg_toplevel_set_activated(prev_toplevel, false);
    }
  }
  struct wlr_keyboard *keyboard = wlr_seat_get_keyboard(seat);
  /* Move the toplevel to the front */
  wlr_scene_node_raise_to_top(&toplevel->scene_tree->node);
  wl_list_remove(&toplevel->link);
  wl_list_insert(&server->toplevels, &toplevel->link);
  /* Activate the new surface */
  wlr_xdg_toplevel_set_activated(toplevel->xdg_toplevel, true);
  /*
   * Tell the seat to have the keyboard enter this surface. wlroots will keep
   * track of this and automatically send key events to the appropriate
   * clients without additional work on your part.
   */
  if (keyboard != NULL) {
    wlr_seat_keyboard_notify_enter(seat, toplevel->xdg_toplevel->base->surface,
                                   keyboard->keycodes, keyboard->num_keycodes,
                                   &keyboard->modifiers);
  }
}
static void layer_surface_destroy(struct wl_listener *listener, void *data);
static void layer_surface_map(struct wl_listener *listener, void *data);
static void layer_surface_unmap(struct wl_listener *listener, void *data);
static void layer_surface_configure(struct wl_listener *listener, void *data);
static void keyboard_handle_modifiers(struct wl_listener *listener,
                                      void *data) {
  /* This event is raised when a modifier key, such as shift or alt, is
   * pressed. We simply communicate this to the client. */
  struct tinywl_keyboard *keyboard =
      wl_container_of(listener, keyboard, modifiers);
  /*
   * A seat can only have one keyboard, but this is a limitation of the
   * Wayland protocol - not wlroots. We assign all connected keyboards to the
   * same seat. You can swap out the underlying wlr_keyboard like this and
   * wlr_seat handles this transparently.
   */
  wlr_seat_set_keyboard(keyboard->server->seat, keyboard->wlr_keyboard);
  /* Send modifiers to the client. */
  wlr_seat_keyboard_notify_modifiers(keyboard->server->seat,
                                     &keyboard->wlr_keyboard->modifiers);
}

bool cycle_windows(struct tinywl_server *server) {
  if (wl_list_length(&server->toplevels) < 2) {
    return false;
  }
  struct tinywl_toplevel *next_toplevel =
      wl_container_of(server->toplevels.prev, next_toplevel, link);
  focus_toplevel(next_toplevel, next_toplevel->xdg_toplevel->base->surface);
  return true;
}
static bool handle_keybinding(struct tinywl_server *server, xkb_keysym_t sym) {
  /*
   * Here we handle compositor keybindings. This is when the compositor is
   * processing keys, rather than passing them on to the client for its own
   * processing.
   *
   * This function assumes Alt is held down.
   */

  switch (sym) {
  case XKB_KEY_Escape:
    wl_display_terminate(server->wl_display);
    break;
  case XKB_KEY_F1:
    bool result = cycle_windows(server);
    /* Cycle to the next toplevel */
    if (!result) {
      wlr_log(WLR_INFO, "Window Cycle Failed, Not enought windows");
    }
    break;
  default:

    if (server->keybinding_handler) {
      wlr_log(WLR_DEBUG, "Calling custom handler");
      server->keybinding_handler(sym);
    }
    return false;
  }
  return true;
}

static void keyboard_handle_key(struct wl_listener *listener, void *data) {
  /* This event is raised when a key is pressed or released. */
  struct tinywl_keyboard *keyboard = wl_container_of(listener, keyboard, key);
  struct tinywl_server *server = keyboard->server;
  struct wlr_keyboard_key_event *event = data;
  struct wlr_seat *seat = server->seat;

  /* Translate libinput keycode -> xkbcommon */
  uint32_t keycode = event->keycode + 8;
  /* Get a list of keysyms based on the keymap for this keyboard */
  const xkb_keysym_t *syms;
  int nsyms =
      xkb_state_key_get_syms(keyboard->wlr_keyboard->xkb_state, keycode, &syms);

  bool handled = false;
  uint32_t modifiers = wlr_keyboard_get_modifiers(keyboard->wlr_keyboard);
  if ((modifiers & global_modifier) &&
      event->state == WL_KEYBOARD_KEY_STATE_PRESSED) {
    /* If alt is held down and this button was _pressed_, we attempt to
     * process it as a compositor keybinding. */

    for (int i = 0; i < nsyms; i++) {
      handled = handle_keybinding(server, syms[i]);
    }
  }

  if (!handled) {
    /* Otherwise, we pass it along to the client. */
    wlr_seat_set_keyboard(seat, keyboard->wlr_keyboard);
    wlr_seat_keyboard_notify_key(seat, event->time_msec, event->keycode,
                                 event->state);
  }
}

void set_keybinding_handler(struct tinywl_server *server,
                            keybinding_handler_t handler) {
  server->keybinding_handler = handler;
}

static void keyboard_handle_destroy(struct wl_listener *listener, void *data) {
  /* This event is raised by the keyboard base wlr_input_device to signal
   * the destruction of the wlr_keyboard. It will no longer receive events
   * and should be destroyed.
   */
  struct tinywl_keyboard *keyboard =
      wl_container_of(listener, keyboard, destroy);
  wl_list_remove(&keyboard->modifiers.link);
  wl_list_remove(&keyboard->key.link);
  wl_list_remove(&keyboard->destroy.link);
  wl_list_remove(&keyboard->link);
  free(keyboard);
}

static void server_new_keyboard(struct tinywl_server *server,
                                struct wlr_input_device *device) {
  struct wlr_keyboard *wlr_keyboard = wlr_keyboard_from_input_device(device);

  struct tinywl_keyboard *keyboard = calloc(1, sizeof(*keyboard));
  keyboard->server = server;
  keyboard->wlr_keyboard = wlr_keyboard;

  /* We need to prepare an XKB keymap and assign it to the keyboard. This
   * assumes the defaults (e.g. layout = "us"). */
  struct xkb_context *context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
  struct xkb_keymap *keymap =
      xkb_keymap_new_from_names(context, NULL, XKB_KEYMAP_COMPILE_NO_FLAGS);

  wlr_keyboard_set_keymap(wlr_keyboard, keymap);
  xkb_keymap_unref(keymap);
  xkb_context_unref(context);
  wlr_keyboard_set_repeat_info(wlr_keyboard, 25, 600);

  /* Here we set up listeners for keyboard events. */
  keyboard->modifiers.notify = keyboard_handle_modifiers;
  wl_signal_add(&wlr_keyboard->events.modifiers, &keyboard->modifiers);
  keyboard->key.notify = keyboard_handle_key;
  wl_signal_add(&wlr_keyboard->events.key, &keyboard->key);
  keyboard->destroy.notify = keyboard_handle_destroy;
  wl_signal_add(&device->events.destroy, &keyboard->destroy);

  wlr_seat_set_keyboard(server->seat, keyboard->wlr_keyboard);

  /* And add the keyboard to our list of keyboards */
  wl_list_insert(&server->keyboards, &keyboard->link);
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

static struct wlr_surface *layer_surface_at(struct tinywl_server *server,
                                            double lx, double ly, double *sx,
                                            double *sy) {
  struct wlr_scene_node *node =
      wlr_scene_node_at(&server->scene->tree.node, lx, ly, sx, sy);
  if (!node || node->type != WLR_SCENE_NODE_BUFFER) {
    return NULL;
  }

  struct wlr_scene_buffer *scene_buffer = wlr_scene_buffer_from_node(node);
  struct wlr_scene_surface *scene_surface =
      wlr_scene_surface_try_from_buffer(scene_buffer);
  if (!scene_surface) {
    return NULL;
  }

  return scene_surface->surface;
}

static struct tinywl_toplevel *desktop_toplevel_at(struct tinywl_server *server,
                                                   double lx, double ly,
                                                   struct wlr_surface **surface,
                                                   double *sx, double *sy) {
  /* This returns the topmost node in the scene at the given layout coords.
   * We only care about surface nodes as we are specifically looking for a
   * surface in the surface tree of a tinywl_toplevel. */
  struct wlr_scene_node *node =
      wlr_scene_node_at(&server->scene->tree.node, lx, ly, sx, sy);
  if (node == NULL || node->type != WLR_SCENE_NODE_BUFFER) {
    return NULL;
  }
  struct wlr_scene_buffer *scene_buffer = wlr_scene_buffer_from_node(node);
  struct wlr_scene_surface *scene_surface =
      wlr_scene_surface_try_from_buffer(scene_buffer);
  if (!scene_surface) {
    return NULL;
  }

  *surface = scene_surface->surface;
  /* Find the node corresponding to the tinywl_toplevel at the root of this
   * surface tree, it is the only one for which we set the data field. */
  struct wlr_scene_tree *tree = node->parent;
  while (tree != NULL && tree->node.data == NULL) {
    tree = tree->node.parent;
  }
  return tree->node.data;
}

static void reset_cursor_mode(struct tinywl_server *server) {
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
  /*
   * Resizing the grabbed toplevel can be a little bit complicated, because we
   * could be resizing from any corner or edge. This not only resizes the
   * toplevel on one or two axes, but can also move the toplevel if you resize
   * from the top or left edges (or top-left corner).
   *
   * Note that some shortcuts are taken here. In a more fleshed-out
   * compositor, you'd wait for the client to prepare a buffer at the new
   * size, then commit any movement that was prepared.
   */
  struct tinywl_toplevel *toplevel = server->grabbed_toplevel;
  double border_x = server->cursor->x - server->grab_x;
  double border_y = server->cursor->y - server->grab_y;
  int new_left = server->grab_geobox.x;
  int new_right = server->grab_geobox.x + server->grab_geobox.width;
  int new_top = server->grab_geobox.y;
  int new_bottom = server->grab_geobox.y + server->grab_geobox.height;

  if (server->resize_edges & WLR_EDGE_TOP) {
    new_top = border_y;
    if (new_top >= new_bottom) {
      new_top = new_bottom - 1;
    }
  } else if (server->resize_edges & WLR_EDGE_BOTTOM) {
    new_bottom = border_y;
    if (new_bottom <= new_top) {
      new_bottom = new_top + 1;
    }
  }
  if (server->resize_edges & WLR_EDGE_LEFT) {
    new_left = border_x;
    if (new_left >= new_right) {
      new_left = new_right - 1;
    }
  } else if (server->resize_edges & WLR_EDGE_RIGHT) {
    new_right = border_x;
    if (new_right <= new_left) {
      new_right = new_left + 1;
    }
  }

  struct wlr_box geo_box;
  wlr_xdg_surface_get_geometry(toplevel->xdg_toplevel->base, &geo_box);
  wlr_scene_node_set_position(&toplevel->scene_tree->node, new_left - geo_box.x,
                              new_top - geo_box.y);

  int new_width = new_right - new_left;
  int new_height = new_bottom - new_top;
  wlr_xdg_toplevel_set_size(toplevel->xdg_toplevel, new_width, new_height);
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
  /* This event is forwarded by the cursor when a pointer emits a button
   * event. */
  struct tinywl_server *server =
      wl_container_of(listener, server, cursor_button);
  struct wlr_pointer_button_event *event = data;
  /* Notify the client with pointer focus that a button press has occurred */
  wlr_seat_pointer_notify_button(server->seat, event->time_msec, event->button,
                                 event->state);
  double sx, sy;
  struct wlr_surface *surface = NULL;
  struct tinywl_toplevel *toplevel = desktop_toplevel_at(
      server, server->cursor->x, server->cursor->y, &surface, &sx, &sy);
  if (event->state == WLR_BUTTON_RELEASED) {
    /* If you released any buttons, we exit interactive move/resize mode. */
    reset_cursor_mode(server);
  } else {
    /* Focus that client if the button was _pressed_ */
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
  /* This function is called every time an output is ready to display a frame,
   * generally at the output's refresh rate (e.g. 60Hz). */
  struct tinywl_output *output = wl_container_of(listener, output, frame);
  struct wlr_scene *scene = output->server->scene;

  struct wlr_scene_output *scene_output =
      wlr_scene_get_scene_output(scene, output->wlr_output);

  /* Render the scene if needed and commit the output */
  wlr_scene_output_commit(scene_output, NULL);

  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
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

static void server_new_output(struct wl_listener *listener, void *data) {
  /* This event is raised by the backend when a new output (aka a display or
   * monitor) becomes available. */
  struct tinywl_server *server = wl_container_of(listener, server, new_output);
  struct wlr_output *wlr_output = data;

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

static void xdg_toplevel_map(struct wl_listener *listener, void *data) {
  /* Called when the surface is mapped, or ready to display on-screen. */
  struct tinywl_toplevel *toplevel = wl_container_of(listener, toplevel, map);

  wl_list_insert(&toplevel->server->toplevels, &toplevel->link);

  focus_toplevel(toplevel, toplevel->xdg_toplevel->base->surface);
}

static void xdg_toplevel_unmap(struct wl_listener *listener, void *data) {
  /* Called when the surface is unmapped, and should no longer be shown. */
  struct tinywl_toplevel *toplevel = wl_container_of(listener, toplevel, unmap);

  /* Reset the cursor mode if the grabbed toplevel was unmapped. */
  if (toplevel == toplevel->server->grabbed_toplevel) {
    reset_cursor_mode(toplevel->server);
  }

  wl_list_remove(&toplevel->link);
}

static void xdg_toplevel_destroy(struct wl_listener *listener, void *data) {
  /* Called when the xdg_toplevel is destroyed. */
  struct tinywl_toplevel *toplevel =
      wl_container_of(listener, toplevel, destroy);

  wl_list_remove(&toplevel->map.link);
  wl_list_remove(&toplevel->unmap.link);
  wl_list_remove(&toplevel->destroy.link);
  wl_list_remove(&toplevel->request_move.link);
  wl_list_remove(&toplevel->request_resize.link);
  wl_list_remove(&toplevel->request_maximize.link);
  wl_list_remove(&toplevel->request_fullscreen.link);

  free(toplevel);
}

static void begin_interactive(struct tinywl_toplevel *toplevel,
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

static void xdg_toplevel_request_move(struct wl_listener *listener,
                                      void *data) {
  /* This event is raised when a client would like to begin an interactive
   * move, typically because the user clicked on their client-side
   * decorations. Note that a more sophisticated compositor should check the
   * provided serial against a list of button press serials sent to this
   * client, to prevent the client from requesting this whenever they want. */
  struct tinywl_toplevel *toplevel =
      wl_container_of(listener, toplevel, request_move);
  begin_interactive(toplevel, TINYWL_CURSOR_MOVE, 0);
}

static void xdg_toplevel_request_resize(struct wl_listener *listener,
                                        void *data) {
  /* This event is raised when a client would like to begin an interactive
   * resize, typically because the user clicked on their client-side
   * decorations. Note that a more sophisticated compositor should check the
   * provided serial against a list of button press serials sent to this
   * client, to prevent the client from requesting this whenever they want. */
  struct wlr_xdg_toplevel_resize_event *event = data;
  struct tinywl_toplevel *toplevel =
      wl_container_of(listener, toplevel, request_resize);
  begin_interactive(toplevel, TINYWL_CURSOR_RESIZE, event->edges);
}

static void xdg_toplevel_request_maximize(struct wl_listener *listener,
                                          void *data) {
  /* This event is raised when a client would like to maximize itself,
   * typically because the user clicked on the maximize button on
   * client-side decorations. tinywl doesn't support maximization, but
   * to conform to xdg-shell protocol we still must send a configure.
   * wlr_xdg_surface_schedule_configure() is used to send an empty reply. */
  struct tinywl_toplevel *toplevel =
      wl_container_of(listener, toplevel, request_maximize);
  wlr_xdg_surface_schedule_configure(toplevel->xdg_toplevel->base);
}

static void xdg_toplevel_request_fullscreen(struct wl_listener *listener,
                                            void *data) {
  /* Just as with request_maximize, we must send a configure here. */
  struct tinywl_toplevel *toplevel =
      wl_container_of(listener, toplevel, request_fullscreen);
  wlr_xdg_surface_schedule_configure(toplevel->xdg_toplevel->base);
}

static void server_new_xdg_surface(struct wl_listener *listener, void *data) {
  /* This event is raised when wlr_xdg_shell receives a new xdg surface from a
   * client, either a toplevel (application window) or popup. */
  struct tinywl_server *server =
      wl_container_of(listener, server, new_xdg_surface);
  struct wlr_xdg_surface *xdg_surface = data;

  /* We must add xdg popups to the scene graph so they get rendered. The
   * wlroots scene graph provides a helper for this, but to use it we must
   * provide the proper parent scene node of the xdg popup. To enable this,
   * we always set the user data field of xdg_surfaces to the corresponding
   * scene node. */
  if (xdg_surface->role == WLR_XDG_SURFACE_ROLE_POPUP) {
    struct wlr_xdg_surface *parent =
        wlr_xdg_surface_try_from_wlr_surface(xdg_surface->popup->parent);
    assert(parent != NULL);
    struct wlr_scene_tree *parent_tree = parent->data;
    xdg_surface->data = wlr_scene_xdg_surface_create(parent_tree, xdg_surface);
    return;
  }
  assert(xdg_surface->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL);

  /* Allocate a tinywl_toplevel for this surface */
  struct tinywl_toplevel *toplevel = calloc(1, sizeof(*toplevel));
  toplevel->server = server;
  toplevel->xdg_toplevel = xdg_surface->toplevel;
  toplevel->scene_tree = wlr_scene_xdg_surface_create(
      &toplevel->server->scene->tree, toplevel->xdg_toplevel->base);
  toplevel->scene_tree->node.data = toplevel;
  xdg_surface->data = toplevel->scene_tree;

  /* Listen to the various events it can emit */
  toplevel->map.notify = xdg_toplevel_map;
  wl_signal_add(&xdg_surface->surface->events.map, &toplevel->map);
  toplevel->unmap.notify = xdg_toplevel_unmap;
  wl_signal_add(&xdg_surface->surface->events.unmap, &toplevel->unmap);
  toplevel->destroy.notify = xdg_toplevel_destroy;
  wl_signal_add(&xdg_surface->events.destroy, &toplevel->destroy);

  /* cotd */
  struct wlr_xdg_toplevel *xdg_toplevel = xdg_surface->toplevel;
  toplevel->request_move.notify = xdg_toplevel_request_move;
  wl_signal_add(&xdg_toplevel->events.request_move, &toplevel->request_move);
  toplevel->request_resize.notify = xdg_toplevel_request_resize;
  wl_signal_add(&xdg_toplevel->events.request_resize,
                &toplevel->request_resize);
  toplevel->request_maximize.notify = xdg_toplevel_request_maximize;
  wl_signal_add(&xdg_toplevel->events.request_maximize,
                &toplevel->request_maximize);
  toplevel->request_fullscreen.notify = xdg_toplevel_request_fullscreen;
  wl_signal_add(&xdg_toplevel->events.request_fullscreen,
                &toplevel->request_fullscreen);
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

static void get_output_dimensions(struct wlr_output *output, int32_t *width,
                                  int32_t *height) {
  if (!output) {
    *width = 800;
    *height = 25;
    return;
  }
  // Ensure dimensions are within INT32 bounds
  *width = output->width > INT32_MAX ? INT32_MAX : output->width;
  *height = output->height > INT32_MAX ? INT32_MAX : output->height;
}

// static void get_output_dimensions(struct wlr_output *output, int *width,
//                                   int *height) {
//   if (!output) {
//     *width = 800; // Default fallback width
//     *height = 25; // Default fallback height
//     return;
//   }
//   *width = output->width;
//   *height = output->height;
// }
static void server_new_layer_surface(struct wl_listener *listener, void *data) {
  struct tinywl_server *server =
      wl_container_of(listener, server, new_layer_surface);
  struct wlr_layer_surface_v1 *wlr_layer_surface = data;

  wlr_log(WLR_DEBUG, "New layer surface request received");
  if (!server->allocator) {
    wlr_log(WLR_ERROR, "No allocator available for layer surface");
    return;
  }
  wlr_log(WLR_DEBUG, "Allocator status: %s",
          server->allocator ? "available" : "not available");
  wlr_log(WLR_DEBUG, "Layer surface details - width: %d, height: %d, layer: %d",
          wlr_layer_surface->pending.desired_width,
          wlr_layer_surface->pending.desired_height,
          wlr_layer_surface->pending.layer);

  // Ensure we have an output
  if (!wlr_layer_surface->output && !wl_list_empty(&server->outputs)) {
    struct tinywl_output *first_output =
        wl_container_of(server->outputs.next, first_output, link);
    wlr_layer_surface->output = first_output->wlr_output;
  }

  // Get output dimensions for defaults
  int32_t output_width = 0, output_height = 0;
  if (wlr_layer_surface->output) {
    output_width = wlr_layer_surface->output->width;
    output_height = wlr_layer_surface->output->height;
  } else {
    // Fallback dimensions if no output
    output_width = 1920; // Default fallback
    output_height = 1080;
  }

  // Set safe initial dimensions
  if (wlr_layer_surface->pending.desired_width == 0) {
    wlr_layer_surface->pending.desired_width = output_width;
    wlr_log(WLR_DEBUG, "Setting default width to output width: %d",
            output_width);
  }
  if (wlr_layer_surface->pending.desired_height == 0) {
    wlr_layer_surface->pending.desired_height = 25;
  }

  // Ensure dimensions are within sane limits
  wlr_layer_surface->pending.desired_width =
      MIN(output_width, MAX(100, wlr_layer_surface->pending.desired_width));
  wlr_layer_surface->pending.desired_height =
      MIN(output_height, MAX(25, wlr_layer_surface->pending.desired_height));

  // Set default anchor if none provided
  if (wlr_layer_surface->pending.anchor == 0) {
    wlr_layer_surface->pending.anchor = ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP |
                                        ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT |
                                        ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT;
  }

  struct tinywl_layer_surface *layer_surface =
      calloc(1, sizeof(*layer_surface));
  if (!layer_surface) {
    wlr_log(WLR_ERROR, "Failed to allocate layer surface");
    return;
  }

  layer_surface->server = server;
  layer_surface->layer_surface = wlr_layer_surface;

  // Create the scene tree node
  struct wlr_scene_layer_surface_v1 *scene_layer =
      wlr_scene_layer_surface_v1_create(&server->scene->tree,
                                        wlr_layer_surface);
  if (!scene_layer) {
    wlr_log(WLR_ERROR, "Failed to create scene layer surface");
    free(layer_surface);
    return;
  }

  layer_surface->scene_tree = scene_layer;

  // Setup listeners
  layer_surface->destroy.notify = layer_surface_destroy;
  wl_signal_add(&wlr_layer_surface->events.destroy, &layer_surface->destroy);

  layer_surface->map.notify = layer_surface_map;
  wl_signal_add(&wlr_layer_surface->surface->events.map, &layer_surface->map);

  layer_surface->unmap.notify = layer_surface_unmap;
  wl_signal_add(&wlr_layer_surface->surface->events.unmap,
                &layer_surface->unmap);

  layer_surface->configure.notify = layer_surface_configure;
  wl_signal_add(&wlr_layer_surface->surface->events.commit,
                &layer_surface->configure);

  // Add to list of layer surfaces
  wl_list_insert(&server->layer_surfaces, &layer_surface->link);

  // Configure surface with validated dimensions
  uint32_t serial = wlr_layer_surface_v1_configure(
      wlr_layer_surface, wlr_layer_surface->pending.desired_width,
      wlr_layer_surface->pending.desired_height);

  wlr_log(WLR_DEBUG, "New layer surface configured with dimensions: %dx%d",
          wlr_layer_surface->pending.desired_width,
          wlr_layer_surface->pending.desired_height);
}
static void layer_surface_configure(struct wl_listener *listener, void *data) {
  struct tinywl_layer_surface *layer_surface =
      wl_container_of(listener, layer_surface, configure);
  struct wlr_layer_surface_v1 *wlr_layer_surface = layer_surface->layer_surface;

  // Get current width and preserve it if it's valid
  uint32_t width = wlr_layer_surface->current.desired_width;
  if (width == 0) {
    // If width is 0, use the output width
    if (wlr_layer_surface->output) {
      width = wlr_layer_surface->output->width;
    } else {
      width = 1920; // Fallback width
    }
  }

  uint32_t height = wlr_layer_surface->current.desired_height;
  if (height == 0) {
    height = 26; // Default height
  }

  wlr_log(WLR_DEBUG, "Layer surface configure with preserved dimensions: %dx%d",
          width, height);

  uint32_t serial =
      wlr_layer_surface_v1_configure(wlr_layer_surface, width, height);
  wlr_log(WLR_DEBUG, "Layer surface configured with serial %d", serial);
}

static void focus_layer_surface(struct tinywl_server *server,
                                struct wlr_surface *surface) {
  if (!surface) {
    return;
  }

  struct wlr_seat *seat = server->seat;
  struct wlr_surface *prev_surface = seat->keyboard_state.focused_surface;

  if (prev_surface == surface) {
    return;
  }

  if (prev_surface) {
    /*
     * Deactivate the previously focused surface if it's a toplevel.
     */
    struct wlr_xdg_toplevel *prev_toplevel =
        wlr_xdg_toplevel_try_from_wlr_surface(prev_surface);
    if (prev_toplevel) {
      wlr_xdg_toplevel_set_activated(prev_toplevel, false);
    }
  }

  /* Activate the new surface */
  struct wlr_keyboard *keyboard = wlr_seat_get_keyboard(seat);
  if (keyboard) {
    wlr_seat_keyboard_notify_enter(seat, surface, keyboard->keycodes,
                                   keyboard->num_keycodes,
                                   &keyboard->modifiers);
  }
}

static void layer_surface_map(struct wl_listener *listener, void *data) {
  struct tinywl_layer_surface *layer_surface =
      wl_container_of(listener, layer_surface, map);
  struct wlr_layer_surface_v1 *wlr_layer_surface = layer_surface->layer_surface;

  // Get output dimensions
  int width, height;
  get_output_dimensions(wlr_layer_surface->output, &width, &height);

  // Ensure dimensions are valid
  if (wlr_layer_surface->current.desired_width <= 0 ||
      wlr_layer_surface->current.desired_width > width) {
    wlr_layer_surface->current.desired_width = width / 2;
  }
  if (wlr_layer_surface->current.desired_height <= 0 ||
      wlr_layer_surface->current.desired_height > height) {
    wlr_layer_surface->current.desired_height = 25;
  }

  // Configure surface with validated dimensions
  wlr_layer_surface_v1_configure(wlr_layer_surface,
                                 wlr_layer_surface->current.desired_width,
                                 wlr_layer_surface->current.desired_height);

  focus_layer_surface(layer_surface->server, wlr_layer_surface->surface);
}

static void layer_surface_unmap(struct wl_listener *listener, void *data) {
  struct tinywl_layer_surface *toplevel =
      wl_container_of(listener, toplevel, unmap);
  wlr_log(WLR_DEBUG, "Layer surface unmapped");
}

static void layer_surface_destroy(struct wl_listener *listener, void *data) {
  struct tinywl_layer_surface *toplevel =
      wl_container_of(listener, toplevel, destroy);

  wlr_log(WLR_DEBUG, "Destroying layer surface");

  wl_list_remove(&toplevel->link);
  wl_list_remove(&toplevel->destroy.link);
  wl_list_remove(&toplevel->map.link);
  wl_list_remove(&toplevel->unmap.link);

  free(toplevel);
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

  // Output and scene setup
  initialize_output_layout(server);
  initialize_scene(server);

  // Input handling setup (seat and cursor)
  initialize_seat(server);
  initialize_cursor(server);

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
  /* Create the display */
  // server->wl_display = wl_display_create();
  // if (!server->wl_display) {
  //   wlr_log(WLR_ERROR, "Could not create wayland display");
  //   return false;
  // }
  //
  wlr_log(WLR_ERROR, "got this far");

  /* Create the backend - let wlroots handle session creation */
  // server->backend = wlr_backend_autocreate(server->wl_display, NULL);
  // if (!server->backend) {
  //   wlr_log(WLR_ERROR, "Failed to create backend");
  //   wl_display_destroy(server->wl_display);
  //   return false;
  // }

  wlr_log(WLR_ERROR, "but not here");

  /* Initialize the renderer */
  // server->renderer = wlr_renderer_autocreate(server->backend);
  // if (!server->renderer) {
  //   wlr_log(WLR_ERROR, "Failed to create renderer");
  //   return false;
  // }
  //
  if (!wlr_renderer_init_wl_display(server->renderer, server->wl_display)) {
    wlr_log(WLR_ERROR, "Failed to initialize renderer with display");
    return false;
  }

  /* Create allocator */
  // server->allocator =
  //     wlr_allocator_autocreate(server->backend, server->renderer);
  // if (!server->allocator) {
  //   wlr_log(WLR_ERROR, "Failed to create allocator");
  //   return false;
  // }

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
