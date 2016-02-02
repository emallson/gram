#pragma once
#include <libguile.h>
#include <wlc/wlc.h>


void gram_keydown_hook_init (void);
void *gram_keydown_hook_run (void *data);
