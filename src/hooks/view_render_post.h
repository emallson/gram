#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

void gram_view_render_post_hook_init (void);
SCM gram_view_render_post_hook_run (void *data);
