#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>

#include <libguile.h>
#include <wlc/wlc.h>

#include "config.h"
#include "types/types.h"
#include "hooks/hooks.h"

static SCM
gram_terminate (void) {
  wlc_terminate();
  return SCM_UNSPECIFIED;
}

static bool
keyboard_key (wlc_handle view, uint32_t time,
              const struct wlc_modifiers *modifiers, uint32_t key,
              enum wlc_key_state state)
{
  (void) time, (void) key;

  struct wlc_modifiers mods;

  memcpy (&mods, modifiers, sizeof (mods));

  struct gram_keysym keysym = {
    .keycode = key,
    .sym = wlc_keyboard_get_keysym_for_key (key, NULL),
    .mods = mods,
    .mouse = false,
    .mouse_button = -1
  };

  if (state == WLC_KEY_STATE_PRESSED)
  {
    struct keydown_input input = {
      .view = view,
      .keysym = keysym
    };
    bool t = (SCM) gram_call_hook (gram_keydown_hook_run, &input) == SCM_BOOL_T;
    gram_swallow = false;
    return t;
  }
  else if (state == WLC_KEY_STATE_RELEASED)
  {
    struct keyup_input input = {
      .view = view,
      .keysym = keysym
    };
    bool t = (SCM) gram_call_hook (gram_keyup_hook_run, &input) == SCM_BOOL_T;
    gram_swallow = false;
    return t;
  }

  return false;
}

static bool
view_created (wlc_handle view)
{
  gram_call_hook(gram_view_created_hook_run, &view);

  return true;
}

static void
view_destroyed (wlc_handle view)
{
  /* free the view and then run the hooks */
  gram_view_deactivate (view);
  gram_call_hook (gram_view_destroyed_hook_run, NULL);
}

static void
view_focus (wlc_handle view, bool focus)
{
  struct view_focus_input input = {
    .handle = view,
    .focus = focus
  };

  wlc_view_set_state (view, WLC_BIT_ACTIVATED, focus);
  gram_call_hook (gram_view_focus_hook_run, &input);
}

static void
view_move_to_output (wlc_handle view, wlc_handle from, wlc_handle to)
{
  struct move_to_output_input input = {
    .view = view,
    .from_out = from,
    .to_out = to
  };

  gram_call_hook (gram_view_move_to_output_hook_run, &input);
}

static void
view_request_geometry (wlc_handle view, const struct wlc_geometry *geo)
{
  struct view_geo s = {
    view, geo
  };

  gram_call_hook (gram_view_request_geometry_hook_run, &s);
}

static void
view_render_pre (wlc_handle view)
{
  gram_call_hook (gram_view_render_pre_hook_run, &view);
}

static void
view_render_post (wlc_handle view)
{
  gram_call_hook (gram_view_render_post_hook_run, &view);
}

static bool
output_created (wlc_handle output)
{
  gram_call_hook (gram_output_created_hook_run, &output);
  return true;
}

static void
output_destroyed (wlc_handle output)
{
  gram_call_hook (gram_output_destroyed_hook_run, &output);
}

static void
output_focus (wlc_handle output, bool focus)
{
  struct output_focus_input input = {
    .handle = output,
    .focus = focus
  };
  gram_call_hook (gram_output_focus_hook_run, &input);
}

static void
output_resolution (wlc_handle output, const struct wlc_size *from,
                   const struct wlc_size *to)
{
  struct resolution_input input = {
    .handle = output,
    .from = from,
    .to = to
  };
  gram_call_hook (gram_output_resolution_hook_run, &input);
}

static void
output_render_pre (wlc_handle output)
{
  gram_call_hook (gram_output_render_pre_hook_run, &output);
}

static void
output_render_post (wlc_handle output)
{
  gram_call_hook (gram_output_render_post_hook_run, &output);
}

static bool
pointer_motion (wlc_handle view, uint32_t time, const struct wlc_point *point)
{
  struct pointer_motion_input input = {
    .view = view,
    .time = time,
    .point = point
  };
  gram_call_hook (gram_pointer_motion_hook_run, &input);
  /* pointer motion always goes to the target view */
  wlc_pointer_set_position (point);
  return false;
}

static bool
pointer_button (wlc_handle view, uint32_t time, const struct wlc_modifiers *modifiers,
                uint32_t button, enum wlc_button_state state,
                const struct wlc_point *point)
{
  struct wlc_modifiers mods;

  memcpy (&mods, modifiers, sizeof (mods));

  struct gram_keysym keysym = {
    .keycode = 0,
    .sym = 0,
    .mods = mods,
    .mouse = true,
    /* the magic number 272 appears to be what mouse button 1 is */
    .mouse_button = button - 271
  };

  if (state == WLC_BUTTON_STATE_PRESSED)
  {
    struct keydown_input input = {
      .view = view,
      .keysym = keysym
    };
    bool t = (SCM) gram_call_hook (gram_keydown_hook_run, &input) == SCM_BOOL_T;
    gram_swallow = false;
    return t;
  }
  else if (state == WLC_BUTTON_STATE_RELEASED)
  {
    struct keyup_input input = {
      .view = view,
      .keysym = keysym
    };
    bool t = (SCM) gram_call_hook (gram_keyup_hook_run, &input) == SCM_BOOL_T;
    gram_swallow = false;
    return t;
  }

  return false;
}

static void
compositor_ready ()
{
  gram_call_hook (gram_compositor_ready_hook_run, NULL);
}

static void
compositor_terminate ()
{
  gram_call_hook (gram_compositor_terminate_hook_run, NULL);
}

static void *
load_init (void *data)
{
  scm_variable_set_x (scm_c_lookup ("%load-path"),
                      scm_append (scm_list_2
                                  (scm_variable_ref
                                   (scm_c_lookup ("%load-path")),
                                   scm_list_1 (scm_from_locale_string
                                               (SCHEME_DIR)))));
  scm_c_primitive_load ((char *) data);
  return SCM_UNSPECIFIED;
}

static void *
init_guile (void *data)
{
  /* enables UTF-8 support */
  scm_setlocale (scm_variable_ref (scm_c_lookup ("LC_ALL")),
                 scm_from_locale_string (""));
  init_gram_types ();
  init_gram_hooks ();
  scm_c_define_gsubr("terminate", 0, 0, 0, gram_terminate);
  return SCM_UNSPECIFIED;
}

static char *
get_init_file (int argc, char **argv)
{
  int opt, len;
  char *home = getenv("HOME"), *init_file = calloc(100, sizeof(char));
  if(home) {
    strncpy(init_file, home, 100);
    strncpy(init_file + strlen(home), "/.gram.d/init.scm", 100 - strlen(home));
  }
  while ((opt = getopt (argc, argv, "i:")) != -1)
  {
    switch (opt)
    {
    case 'i':
      len = strlen (optarg);
      free(init_file);
      init_file = calloc (len, sizeof (char));
      strncpy (init_file, optarg, len);
      break;
    }
  }
  printf("Init file: %s\n", init_file);
  return init_file;
}

void
logger (enum wlc_log_type type, const char *str)
{
  printf ("%s\n", str);
}

int
main (int argc, char **argv)
{
  wlc_log_set_handler (logger);

  wlc_set_output_created_cb (output_created);   // Done - Untested
  wlc_set_output_destroyed_cb (output_destroyed);       // Done - Untested
  wlc_set_output_focus_cb (output_focus);       // Done - Untested
  wlc_set_output_resolution_cb (output_resolution);     // Done - Untested
  wlc_set_output_render_pre_cb (output_render_pre);     // Done - Untested
  wlc_set_output_render_post_cb (output_render_post);   // Done - Untested
  wlc_set_view_created_cb (view_created);       // Done
  wlc_set_view_destroyed_cb (view_destroyed);   // Done - Untested
  wlc_set_view_focus_cb (view_focus);   // Done - Untested
  wlc_set_view_move_to_output_cb (view_move_to_output); // Done - Untested
  /* punting on these for the moment */
  wlc_set_view_request_geometry_cb (view_request_geometry);
  /* wlc_set_state_cb (view_request_state); */
  /* wlc_set_move_cb (view_request_move); */
  /* wlc_set_resize_cb (view_request_resize); */
  wlc_set_view_render_pre_cb (view_render_pre); // Done - Untested
  wlc_set_view_render_post_cb (view_render_post);       // Done - Untested
  wlc_set_keyboard_key_cb (keyboard_key);       // Done - should add keyup
  /* the pointer_button and pointer_scroll events should be tied into the key
     system e.g. (kbd "M-Mouse1") (kbd "M-ScrollUp") */
  wlc_set_pointer_button_cb (pointer_button);
  /* wlc_set_pointer_scroll_cb (pointer_scroll); */
  wlc_set_pointer_motion_cb (pointer_motion);   // Done - untested
  /* this .touch should also be tied into the key system */
  /* wlc_set_touch_touch_cb (touch_touch); */
  wlc_set_compositor_ready_cb (compositor_ready);
  wlc_set_compositor_terminate_cb (compositor_terminate);
  /* Experimental -- Don't see need for at the moment */
  /* wlc_set_input_created_cb (input_created); */
  /* wlc_set_input_destroyed_cb (input_destroyed); */

  if (!wlc_init ())
    return EXIT_FAILURE;

  char *init_file = get_init_file (argc, argv);
  if (access (init_file, F_OK | R_OK) == -1)
  {
    init_file = NULL;
  }

  scm_with_guile (init_guile, (void *) NULL);

  if (init_file != NULL)
  {
    scm_with_guile (load_init, init_file);
  }

  wlc_run ();
  return EXIT_SUCCESS;
}
