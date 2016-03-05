#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

struct output_focus_input {
  wlc_handle handle;
  bool focus;
};

void gram_output_focus_hook_init (void);
void *gram_output_focus_hook_run (void *data);
