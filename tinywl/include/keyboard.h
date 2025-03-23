#ifndef KEYBOARD_H
#define KEYBOARD_H
#include "input.h"
#include "wayland-server-core.h"
#include <wlr/types/wlr_keyboard.h>

void keyboard_handle_modifiers(struct wl_listener *listener, void *data);
void keyboard_handle_key(struct wl_listener *listener, void *data);
bool handle_keybinding(struct tinywl_server *server, xkb_keysym_t sym);
void set_keybinding_handler(struct tinywl_server *server,
                            keybinding_handler_t handler);
void keyboard_handle_destroy(struct wl_listener *listener, void *data);
void server_new_keyboard(struct tinywl_server *server,
                         struct wlr_input_device *device);

#endif
