#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

void gram_compositor_terminate_hook_init (void);
void *gram_compositor_terminate_hook_run (void *data);
