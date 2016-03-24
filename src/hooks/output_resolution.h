#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

struct resolution_input
{
  wlc_handle handle;
  const struct wlc_size *from;
  const struct wlc_size *to;
};

void gram_output_resolution_hook_init (void);
void *gram_output_resolution_hook_run (void *data);
