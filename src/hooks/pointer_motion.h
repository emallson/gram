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
SCM gram_pointer_motion_hook_run (void *data);

/* TODO: move this to its own file. Leaving it here for now because I
 * don't want to add all that infrastructure for a single function. */
void gram_pointer_fns_init (void);
