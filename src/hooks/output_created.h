#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

void gram_output_created_hook_init (void);
void *gram_output_created_hook_run (void *data);