#include<libguile.h>

#include "hooks.h"

static void
init_gram_key_hooks (void *ignore)
{
  gram_keydown_hook_init ();
}

static void
init_gram_view_hooks (void *ignore)
{
  gram_view_created_hook_init ();
  gram_view_destroyed_hook_init ();
  gram_view_focus_hook_init ();
  gram_view_move_to_output_hook_init ();
  gram_view_render_pre_hook_init ();
  gram_view_render_post_hook_init ();
  gram_view_request_geometry_hook_init ();
}

static void
init_gram_output_hooks (void *ignore)
{
  gram_output_created_hook_init ();
  gram_output_destroyed_hook_init ();
  gram_output_focus_hook_init ();
  gram_output_render_pre_hook_init ();
  gram_output_render_post_hook_init ();
  gram_output_resolution_hook_init ();
}

static void
init_gram_pointer_hooks (void *ignore)
{
  gram_pointer_motion_hook_init ();
}

static void
init_gram_compositor_hooks (void *ignore)
{
  gram_compositor_ready_hook_init ();
  gram_compositor_terminate_hook_init ();
}

void
init_gram_hooks (void)
{
  scm_c_define_module ("gram keysym hooks", init_gram_key_hooks, NULL);
  scm_c_define_module ("gram view hooks", init_gram_view_hooks, NULL);
  scm_c_define_module ("gram output hooks", init_gram_output_hooks, NULL);
  scm_c_define_module ("gram pointer hooks", init_gram_pointer_hooks, NULL);
  scm_c_define_module ("gram compositor hooks", init_gram_compositor_hooks,
                       NULL);
}
