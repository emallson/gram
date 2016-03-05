#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

struct move_to_output_input {
  wlc_handle view;
  wlc_handle from_out;
  wlc_handle to_out;
};

void gram_view_move_to_output_hook_init (void);
void *gram_view_move_to_output_hook_run (void *data);
