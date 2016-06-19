#include<libguile.h>

#include "hooks.h"

static void
init_gram_key_hooks (void *ignore)
{
  gram_keydown_hook_init ();
  gram_keyup_hook_init ();
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
init_gram_pointer (void *ignore)
{
  /* defined in pointer_motion.h */
  gram_pointer_fns_init();
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
  /* this is out of place but I don't have a better spot for it right
   * now. */

  scm_c_define_module ("gram pointer", init_gram_pointer, NULL);
}

struct gram_hook_wrapper {
  SCM (*hook)(void*);
  void* data;
};

static SCM
gram_hook_error_handler(void* data, SCM key, SCM args) {
  return scm_simple_format(SCM_BOOL_T, scm_from_locale_string("~A error thrown with arguments ~A\n"), scm_list_2(key, args));
}

static void*
gram_call_hook_body(void* data) {
  struct gram_hook_wrapper hook = * (struct gram_hook_wrapper*) data;
  return (void*)scm_internal_catch(SCM_BOOL_T, hook.hook, hook.data,
                                   gram_hook_error_handler, NULL);
}

void*
gram_call_hook(scm_t_catch_body hook, void* data) {
  struct gram_hook_wrapper wrapper = {
    .hook = hook,
    .data = data
  };
  return scm_with_guile(gram_call_hook_body, &wrapper);
}
