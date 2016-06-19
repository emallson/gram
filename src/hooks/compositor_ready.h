#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

void gram_compositor_ready_hook_init (void);
SCM gram_compositor_ready_hook_run (void *data);
