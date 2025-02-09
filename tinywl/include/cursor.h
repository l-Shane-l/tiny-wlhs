#ifndef CURSOR_H
#define CURSOR_H

struct wlr_cursor;

enum tinywl_cursor_mode {
  TINYWL_CURSOR_PASSTHROUGH,
  TINYWL_CURSOR_MOVE,
  TINYWL_CURSOR_RESIZE,
};

#endif
