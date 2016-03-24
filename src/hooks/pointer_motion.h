#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

struct pointer_motion_input
{
  wlc_handle view;
  uint32_t time;
  const struct wlc_point *point;
};

void gram_pointer_motion_hook_init (void);
void *gram_pointer_motion_hook_run (void *data);
