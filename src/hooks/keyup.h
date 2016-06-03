#pragma once
#include <libguile.h>
#include <wlc/wlc.h>


void gram_keyup_hook_init (void);
void *gram_keyup_hook_run (void *data);
