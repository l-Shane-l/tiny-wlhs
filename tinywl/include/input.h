#ifndef INPUT_H
#define INPUT_H

#include "wayland-server-core.h"
#include "xkbcommon/xkbcommon.h"
#include <wlr/types/wlr_keyboard.h>

typedef void (*keybinding_handler_t)(xkb_keysym_t sym);

static uint32_t global_modifier = WLR_MODIFIER_LOGO; // Default

void set_modifier_key(uint32_t modifier);

struct tinywl_keyboard {
  struct wl_list link;
  struct tinywl_server *server;
  struct wlr_keyboard *wlr_keyboard;
  struct wl_listener modifiers;
  struct wl_listener key;
  struct wl_listener destroy;
};

#endif
