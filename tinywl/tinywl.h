#ifndef TINYWL_H
#define TINYWL_H

#include <stdint.h>

// Forward declarations for opaque pointers
struct wl_display;
struct wlr_backend;
struct wlr_renderer;
struct wlr_allocator;
struct wlr_scene;
struct wlr_scene_output_layout;
struct wlr_xdg_shell;
struct wlr_cursor;
struct wlr_xcursor_manager;
struct wlr_seat;
struct tinywl_toplevel;
struct wlr_output_layout;

// Define structures that were causing "incomplete type" errors
struct wl_listener {
    void *notify;
};

struct wl_list {
    void *prev;
    void *next;
};

struct wlr_box {
    int x, y;
    int width, height;
};

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
};

#endif // TINYWL_H
