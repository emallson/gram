#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

#include "../types/keysym.h"

struct keyup_input
{
  const wlc_handle view;
  struct gram_keysym keysym;
};

void gram_keyup_hook_init (void);
void *gram_keyup_hook_run (void *data);
