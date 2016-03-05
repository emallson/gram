#include "hooks.h"

void
init_gram_hooks (void)
{
  gram_keydown_hook_init ();
  gram_view_created_hook_init ();
  gram_view_destroyed_hook_init ();
  gram_view_focus_hook_init ();
  gram_view_move_to_output_hook_init ();
  gram_view_render_pre_hook_init ();
  gram_view_render_post_hook_init ();

  gram_output_created_hook_init ();
  gram_output_destroyed_hook_init ();
  gram_output_focus_hook_init ();
  gram_output_render_pre_hook_init ();
  gram_output_render_post_hook_init ();
  gram_output_resolution_hook_init ();

  gram_pointer_motion_hook_init();

  gram_compositor_ready_hook_init();
  gram_compositor_terminate_hook_init();
}
