#include "output.h"
#include "server.h"
#include <stdint.h>

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

void get_output_dimensions(struct wlr_output *output, int32_t *width,
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
