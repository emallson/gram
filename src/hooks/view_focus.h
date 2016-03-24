#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

struct view_focus_input
{
  wlc_handle handle;
  bool focus;
};

void gram_view_focus_hook_init (void);
void *gram_view_focus_hook_run (void *data);
