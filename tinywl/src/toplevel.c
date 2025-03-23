#include "toplevel.h"
#include "tinywl.h"
#include <stdlib.h>

void focus_toplevel(struct tinywl_toplevel *toplevel,
                    struct wlr_surface *surface) {
  if (toplevel == NULL) {
    return;
  }
  struct tinywl_server *server = toplevel->server;
  struct wlr_seat *seat = server->seat;
  struct wlr_surface *prev_surface = seat->keyboard_state.focused_surface;

  if (prev_surface == surface) {
    return;
  }

  if (prev_surface) {
    struct wlr_xdg_toplevel *prev_toplevel =
        wlr_xdg_toplevel_try_from_wlr_surface(prev_surface);
    if (prev_toplevel != NULL) {
      wlr_xdg_toplevel_set_activated(prev_toplevel, false);
    }
  }

  // Raise both the window and its borders
  wlr_scene_node_raise_to_top(&toplevel->scene_tree->node);
  wlr_scene_node_raise_to_top(&toplevel->border_tree->node);

  wlr_xdg_toplevel_set_activated(toplevel->xdg_toplevel, true);
  set_border_color(toplevel, true);

  struct wlr_keyboard *keyboard = wlr_seat_get_keyboard(seat);
  if (keyboard != NULL) {
    wlr_seat_keyboard_notify_enter(seat, toplevel->xdg_toplevel->base->surface,
                                   keyboard->keycodes, keyboard->num_keycodes,
                                   &keyboard->modifiers);
  }
}

void set_border_color(struct tinywl_toplevel *toplevel, bool focused) {
  if (!toplevel->scene_tree) {
    return;
  }

  // Set border color based on focus state
  float color[4];
  if (focused) {
    // Bright red for focused
    color[0] = 1.0f; // R
    color[1] = 0.0f; // G
    color[2] = 0.0f; // B
    color[3] = 1.0f; // A
  } else {
    // Bright blue for unfocused
    color[0] = 0.0f; // R
    color[1] = 0.0f; // G
    color[2] = 1.0f; // B
    color[3] = 1.0f; // A
  }

  // Apply the border color
  wlr_scene_node_set_enabled(&toplevel->border_top->node, true);
  wlr_scene_node_set_enabled(&toplevel->border_bottom->node, true);
  wlr_scene_node_set_enabled(&toplevel->border_left->node, true);
  wlr_scene_node_set_enabled(&toplevel->border_right->node, true);

  wlr_scene_rect_set_color(toplevel->border_top, color);
  wlr_scene_rect_set_color(toplevel->border_bottom, color);
  wlr_scene_rect_set_color(toplevel->border_left, color);
  wlr_scene_rect_set_color(toplevel->border_right, color);
}

void update_border_position(struct tinywl_toplevel *toplevel) {
  if (!toplevel->scene_tree) {
    return;
  }

  struct wlr_box geo_box;
  wlr_xdg_surface_get_geometry(toplevel->xdg_toplevel->base, &geo_box);

  const int border_thickness =
      5; // Adjust this value for thicker/thinner borders

  // Position the borders starting OUTSIDE the window bounds
  // This ensures they're visible even at screen edges
  wlr_scene_node_set_position(&toplevel->border_top->node,
                              -border_thickness,  // Start left of window
                              -border_thickness); // Start above window

  wlr_scene_node_set_position(&toplevel->border_bottom->node,
                              -border_thickness, // Start left of window
                              geo_box.height);   // Start at bottom of window

  wlr_scene_node_set_position(&toplevel->border_left->node,
                              -border_thickness,  // Start left of window
                              -border_thickness); // Start above window

  wlr_scene_node_set_position(&toplevel->border_right->node,
                              geo_box.width, // Start at right edge of window
                              -border_thickness); // Start above window

  // Set the size of the borders to extend beyond window bounds
  wlr_scene_rect_set_size(toplevel->border_top,
                          geo_box.width +
                              (border_thickness * 2), // Extra width for corners
                          border_thickness);

  wlr_scene_rect_set_size(toplevel->border_bottom,
                          geo_box.width +
                              (border_thickness * 2), // Extra width for corners
                          border_thickness);

  wlr_scene_rect_set_size(
      toplevel->border_left, border_thickness,
      geo_box.height + (border_thickness * 2)); // Extra height for corners

  wlr_scene_rect_set_size(
      toplevel->border_right, border_thickness,
      geo_box.height + (border_thickness * 2)); // Extra height for corners
}

void xdg_toplevel_request_move(struct wl_listener *listener, void *data) {
  /* This event is raised when a client would like to begin an interactive
   * move, typically because the user clicked on their client-side
   * decorations. Note that a more sophisticated compositor should check the
   * provided serial against a list of button press serials sent to this
   * client, to prevent the client from requesting this whenever they want. */
  struct tinywl_toplevel *toplevel =
      wl_container_of(listener, toplevel, request_move);
  begin_interactive(toplevel, TINYWL_CURSOR_MOVE, 0);
}

void xdg_toplevel_request_resize(struct wl_listener *listener, void *data) {
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

void xdg_toplevel_request_maximize(struct wl_listener *listener, void *data) {
  /* This event is raised when a client would like to maximize itself,
   * typically because the user clicked on the maximize button on
   * client-side decorations. tinywl doesn't support maximization, but
   * to conform to xdg-shell protocol we still must send a configure.
   * wlr_xdg_surface_schedule_configure() is used to send an empty reply. */
  struct tinywl_toplevel *toplevel =
      wl_container_of(listener, toplevel, request_maximize);
  wlr_xdg_surface_schedule_configure(toplevel->xdg_toplevel->base);
}

void xdg_toplevel_request_fullscreen(struct wl_listener *listener, void *data) {
  /* Just as with request_maximize, we must send a configure here. */
  struct tinywl_toplevel *toplevel =
      wl_container_of(listener, toplevel, request_fullscreen);
  wlr_xdg_surface_schedule_configure(toplevel->xdg_toplevel->base);
}

void xdg_toplevel_map(struct wl_listener *listener, void *data) {
  struct tinywl_toplevel *toplevel = wl_container_of(listener, toplevel, map);
  struct wlr_output *output = wlr_output_layout_output_at(
      toplevel->server->output_layout, toplevel->server->cursor->x,
      toplevel->server->cursor->y);

  struct wlr_box full_area = {0};
  wlr_output_effective_resolution(output, &full_area.width, &full_area.height);
  const int border_thickness = 5;

  // Calculate initial usable area
  int usable_width = full_area.width - (2 * border_thickness);
  int usable_height = full_area.height - (2 * border_thickness);

  // Account for layer surfaces (like yambar)
  struct tinywl_layer_surface *layer_surface;
  wl_list_for_each(layer_surface, &toplevel->server->layer_surfaces, link) {
    struct wlr_layer_surface_v1 *wlr_layer_surface =
        layer_surface->layer_surface;

    if (wlr_layer_surface->output != output ||
        !wlr_layer_surface->surface->mapped ||
        wlr_layer_surface->current.exclusive_zone <= 0) {
      continue;
    }

    uint32_t anchor = wlr_layer_surface->current.anchor;
    int32_t exclusive_zone = wlr_layer_surface->current.exclusive_zone;

    if (anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP) {
      usable_height -= exclusive_zone;
    } else if (anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM) {
      usable_height -= exclusive_zone;
    }
  }

  // Set the window size to use the full usable area
  wlr_xdg_toplevel_set_size(toplevel->xdg_toplevel, usable_width,
                            usable_height);

  wl_list_insert(&toplevel->server->toplevels, &toplevel->link);
  update_border_position(toplevel);
  set_border_color(toplevel, false);

  wlr_scene_node_set_enabled(&toplevel->border_top->node, true);
  wlr_scene_node_set_enabled(&toplevel->border_bottom->node, true);
  wlr_scene_node_set_enabled(&toplevel->border_left->node, true);
  wlr_scene_node_set_enabled(&toplevel->border_right->node, true);

  focus_toplevel(toplevel, toplevel->xdg_toplevel->base->surface);
}

void xdg_toplevel_unmap(struct wl_listener *listener, void *data) {
  /* Called when the surface is unmapped, and should no longer be shown. */
  struct tinywl_toplevel *toplevel = wl_container_of(listener, toplevel, unmap);

  /* Reset the cursor mode if the grabbed toplevel was unmapped. */
  if (toplevel == toplevel->server->grabbed_toplevel) {
    reset_cursor_mode(toplevel->server);
  }

  wl_list_remove(&toplevel->link);
}

void xdg_toplevel_destroy(struct wl_listener *listener, void *data) {
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
  wl_list_remove(&toplevel->commit.link); // Add this line

  free(toplevel);
}

void handle_toplevel_commit(struct wl_listener *listener, void *data) {
  struct tinywl_toplevel *toplevel =
      wl_container_of(listener, toplevel, commit);
  update_border_position(toplevel);
}
