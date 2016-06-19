#pragma once
#include <libguile.h>
#include <wlc/wlc.h>
#include "../types/keysym.h"

struct keydown_input
{
  const wlc_handle view;
  struct gram_keysym keysym;
};

void gram_keydown_hook_init (void);
SCM gram_keydown_hook_run (void *data);
