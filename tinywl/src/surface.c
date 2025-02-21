#include "surface.h"
#include "output.h"
#include "tinywl.h"
#include <stdlib.h>

void update_usable_area(struct tinywl_server *server,
                        struct wlr_output *output) {
  struct wlr_box full_area = {0};
  wlr_output_effective_resolution(output, &full_area.width, &full_area.height);

  const int border_thickness = 5; // Define border thickness

  // Start with no reserved space, but account for borders
  int usable_x = border_thickness;
  int usable_y = border_thickness;
  int usable_width = full_area.width - (2 * border_thickness);
  int usable_height = full_area.height - (2 * border_thickness);

  // For each layer surface on this output, adjust based on exclusive zones
  struct tinywl_layer_surface *layer_surface;
  wl_list_for_each(layer_surface, &server->layer_surfaces, link) {
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
      usable_y += exclusive_zone;
      usable_height -= exclusive_zone;
    } else if (anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM) {
      usable_height -= exclusive_zone;
    }
  }

  // Apply the usable area to the xdg_shell_tree
  wlr_scene_node_set_position(&server->xdg_shell_tree->node, usable_x,
                              usable_y);
}

void focus_layer_surface(struct tinywl_server *server,
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

void layer_surface_map(struct wl_listener *listener, void *data) {
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

void layer_surface_unmap(struct wl_listener *listener, void *data) {
  struct tinywl_layer_surface *toplevel =
      wl_container_of(listener, toplevel, unmap);
  wlr_log(WLR_DEBUG, "Layer surface unmapped");
}

void layer_surface_destroy(struct wl_listener *listener, void *data) {
  struct tinywl_layer_surface *toplevel =
      wl_container_of(listener, toplevel, destroy);

  wlr_log(WLR_DEBUG, "Destroying layer surface");

  wl_list_remove(&toplevel->link);
  wl_list_remove(&toplevel->destroy.link);
  wl_list_remove(&toplevel->map.link);
  wl_list_remove(&toplevel->unmap.link);

  free(toplevel);
}

void layer_surface_configure(struct wl_listener *listener, void *data) {
  struct tinywl_layer_surface *layer_surface =
      wl_container_of(listener, layer_surface, configure);
  struct wlr_layer_surface_v1 *wlr_layer_surface = layer_surface->layer_surface;
  struct wlr_output *output = wlr_layer_surface->output;

  if (!output) {
    return;
  }

  uint32_t width = wlr_layer_surface->current.desired_width;
  uint32_t height = wlr_layer_surface->current.desired_height;

  // Calculate width/height if they're set to zero
  if (width == 0) {
    width = output->width;
    if (wlr_layer_surface->current.anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT)
      width -= wlr_layer_surface->current.margin.left;
    if (wlr_layer_surface->current.anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT)
      width -= wlr_layer_surface->current.margin.right;
  }
  if (height == 0) {
    height = output->height;
    if (wlr_layer_surface->current.anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP)
      height -= wlr_layer_surface->current.margin.top;
    if (wlr_layer_surface->current.anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM)
      height -= wlr_layer_surface->current.margin.bottom;
  }

  wlr_layer_surface_v1_configure(wlr_layer_surface, width, height);

  // Update exclusive zones and usable area
  update_usable_area(layer_surface->server, output);
}
