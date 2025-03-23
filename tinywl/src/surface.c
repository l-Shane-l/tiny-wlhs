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

struct wlr_surface *layer_surface_at(struct tinywl_server *server, double lx,
                                     double ly, double *sx, double *sy) {
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

void server_new_layer_surface(struct wl_listener *listener, void *data) {
  struct tinywl_server *server =
      wl_container_of(listener, server, new_layer_surface);
  struct wlr_layer_surface_v1 *layer_surface = data;

  if (!layer_surface->output && !wl_list_empty(&server->outputs)) {
    struct tinywl_output *first_output =
        wl_container_of(server->outputs.next, first_output, link);
    layer_surface->output = first_output->wlr_output;
  }

  wlr_log(WLR_DEBUG, "New layer surface: namespace %s layer %d",
          layer_surface->namespace, layer_surface->pending.layer);

  // Pick the appropriate scene tree
  struct wlr_scene_tree *parent;
  switch (layer_surface->pending.layer) {
  case ZWLR_LAYER_SHELL_V1_LAYER_BACKGROUND:
    parent = server->layer_tree_background;
    break;
  case ZWLR_LAYER_SHELL_V1_LAYER_BOTTOM:
    parent = server->layer_tree_bottom;
    break;
  case ZWLR_LAYER_SHELL_V1_LAYER_TOP:
    parent = server->layer_tree_top;
    break;
  case ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY:
    parent = server->layer_tree_overlay;
    break;
  default:
    wlr_log(WLR_ERROR, "Invalid layer surface layer %d",
            layer_surface->pending.layer);
    return;
  }

  struct tinywl_layer_surface *surface = calloc(1, sizeof(*surface));
  surface->server = server;
  surface->layer_surface = layer_surface;

  // Create scene layer surface
  surface->scene_tree =
      wlr_scene_layer_surface_v1_create(parent, layer_surface);
  if (!surface->scene_tree) {
    free(surface);
    return;
  }

  layer_surface->data = surface->scene_tree;

  surface->destroy.notify = layer_surface_destroy;
  wl_signal_add(&layer_surface->events.destroy, &surface->destroy);

  surface->map.notify = layer_surface_map;
  wl_signal_add(&layer_surface->surface->events.map, &surface->map);

  surface->unmap.notify = layer_surface_unmap;
  wl_signal_add(&layer_surface->surface->events.unmap, &surface->unmap);

  surface->configure.notify = layer_surface_configure;
  wl_signal_add(&layer_surface->surface->events.commit,
                &surface->configure); // NOT surface->events.configure// The
                                      // event is directly on the layer surface/
                                      // Changed from surface->events.commit

  wl_list_insert(&server->layer_surfaces, &surface->link);
}

static void
apply_layer_surface_anchoring(struct tinywl_layer_surface *surface) {
  struct wlr_layer_surface_v1 *wlr_layer_surface = surface->layer_surface;
  struct wlr_output *output = wlr_layer_surface->output;

  if (!output) {
    return;
  }

  int x = 0, y = 0;
  int width = wlr_layer_surface->current.desired_width;
  int height = wlr_layer_surface->current.desired_height;
  uint32_t anchor = wlr_layer_surface->current.anchor;

  if ((anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT) &&
      (anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT)) {
    x = (output->width - width) / 2;
  } else if (anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT) {
    x = output->width - width;
  }

  if ((anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP) &&
      (anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM)) {
    y = (output->height - height) / 2;
  } else if (anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM) {
    y = output->height - height;
  }

  x += wlr_layer_surface->current.margin.left;
  y += wlr_layer_surface->current.margin.top;

  struct wlr_scene_tree *scene_tree = surface->scene_tree->tree;
  wlr_scene_node_set_position(&scene_tree->node, x, y);
}

void server_new_xdg_surface(struct wl_listener *listener, void *data) {
  struct tinywl_server *server =
      wl_container_of(listener, server, new_xdg_surface);
  struct wlr_xdg_surface *xdg_surface = data;

  wlr_log(WLR_DEBUG, "Creating new XDG surface");

  if (xdg_surface->role == WLR_XDG_SURFACE_ROLE_POPUP) {
    struct wlr_xdg_surface *parent =
        wlr_xdg_surface_try_from_wlr_surface(xdg_surface->popup->parent);
    assert(parent != NULL);
    struct wlr_scene_tree *parent_tree = parent->data;
    xdg_surface->data = wlr_scene_xdg_surface_create(parent_tree, xdg_surface);
    wlr_log(WLR_DEBUG, "Created popup surface");
    return;
  }

  assert(xdg_surface->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL);

  struct tinywl_toplevel *toplevel = calloc(1, sizeof(*toplevel));
  toplevel->server = server;
  toplevel->xdg_toplevel = xdg_surface->toplevel;

  // Create scene tree first
  toplevel->scene_tree = wlr_scene_xdg_surface_create(
      server->xdg_shell_tree, toplevel->xdg_toplevel->base);
  if (!toplevel->scene_tree) {
    wlr_log(WLR_ERROR, "Failed to create scene tree for toplevel");
    free(toplevel);
    return;
  }

  // Create border tree as child of scene tree
  toplevel->border_tree = wlr_scene_tree_create(toplevel->scene_tree);
  if (!toplevel->border_tree) {
    wlr_log(WLR_ERROR, "Failed to create border tree");
    free(toplevel);
    return;
  }

  // Create border rectangles in border tree
  const float white[4] = {1.0f, 1.0f, 1.0f, 1.0f};
  toplevel->border_top =
      wlr_scene_rect_create(toplevel->border_tree, 0, 0, white);
  toplevel->border_bottom =
      wlr_scene_rect_create(toplevel->border_tree, 0, 0, white);
  toplevel->border_left =
      wlr_scene_rect_create(toplevel->border_tree, 0, 0, white);
  toplevel->border_right =
      wlr_scene_rect_create(toplevel->border_tree, 0, 0, white);

  // Setup event listeners
  toplevel->map.notify = xdg_toplevel_map;
  wl_signal_add(&xdg_surface->surface->events.map, &toplevel->map);

  toplevel->unmap.notify = xdg_toplevel_unmap;
  wl_signal_add(&xdg_surface->surface->events.unmap, &toplevel->unmap);

  toplevel->destroy.notify = xdg_toplevel_destroy;
  wl_signal_add(&xdg_surface->events.destroy, &toplevel->destroy);

  // Add commit listener
  toplevel->commit.notify = handle_toplevel_commit;
  wl_signal_add(&xdg_surface->surface->events.commit, &toplevel->commit);

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

  toplevel->scene_tree->node.data = toplevel;
  xdg_surface->data = toplevel->scene_tree;

  update_border_position(toplevel);
}
