#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>

#include <libguile.h>
#include <wlc/wlc.h>

#include "config.h"
#include "types/types.h"
#include "hooks/hooks.h"

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
    .mods = mods
  };

  if (state == WLC_KEY_STATE_PRESSED)
  {
    bool *t = (bool *) scm_with_guile (gram_keydown_hook_run, &keysym);
    gram_swallow = false;
    return t == NULL || *t;
  }

  return false;
}

static bool
view_created (wlc_handle view)
{
  wlc_view_set_mask (view, wlc_output_get_mask (wlc_view_get_output (view)));
  wlc_view_bring_to_front (view);
  wlc_view_focus (view);

  const struct wlc_size *r;
  /* todo: error checking */
  r = wlc_output_get_resolution (wlc_view_get_output (view));
  struct wlc_geometry g = {
    {0, 0},
    {r->w, r->h}
  };

  wlc_view_set_geometry (view, 0, &g);

  scm_with_guile (gram_view_created_hook_run, &view);

  return true;
}

static void
view_destroyed (wlc_handle view)
{
  /* run the hook before freeing -- Unsure on correctness -- Investigate */
  scm_with_guile (gram_view_destroyed_hook_run, &view);
  gram_view_deactivate (view);
}

static void
view_focus (wlc_handle view, bool focus)
{
  struct view_focus_input input = {
    .handle = view,
    .focus = focus
  };

  wlc_view_set_state (view, WLC_BIT_ACTIVATED, focus);
  scm_with_guile (gram_view_focus_hook_run, &input);
}

static void
view_move_to_output (wlc_handle view, wlc_handle from, wlc_handle to)
{
  struct move_to_output_input input = {
    .view = view,
    .from_out = from,
    .to_out = to
  };

  scm_with_guile (gram_view_move_to_output_hook_run, &input);
}

static void
view_render_pre (wlc_handle view)
{
  scm_with_guile (gram_view_render_pre_hook_run, &view);
}

static void
view_render_post (wlc_handle view)
{
  scm_with_guile (gram_view_render_post_hook_run, &view);
}

static bool
output_created (wlc_handle output)
{
  scm_with_guile (gram_output_created_hook_run, &output);
  return true;
}

static void
output_destroyed (wlc_handle output)
{
  scm_with_guile (gram_output_destroyed_hook_run, &output);
}

static void
output_focus (wlc_handle output, bool focus)
{
  struct output_focus_input input = {
    .handle = output,
    .focus = focus
  };
  scm_with_guile (gram_output_focus_hook_run, &input);
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
  scm_with_guile (gram_output_resolution_hook_run, &input);
}

static void
output_render_pre (wlc_handle output)
{
  scm_with_guile (gram_output_render_pre_hook_run, &output);
}

static void
output_render_post (wlc_handle output)
{
  scm_with_guile (gram_output_render_post_hook_run, &output);
}

static bool
pointer_motion (wlc_handle view, uint32_t time, const struct wlc_point *point)
{
  struct pointer_motion_input input = {
    .view = view,
    .time = time,
    .point = point
  };
  scm_with_guile (gram_pointer_motion_hook_run, &input);
  /* pointer motion always goes to the target view */
  return false;
}

static void
compositor_ready ()
{
  scm_with_guile (gram_compositor_ready_hook_run, NULL);
}

static void
compositor_terminate ()
{
  scm_with_guile (gram_compositor_terminate_hook_run, NULL);
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
  return SCM_UNSPECIFIED;
}

static char *
get_init_file (int argc, char **argv)
{
  int opt, len;
  char *init_file = "init.scm";
  while ((opt = getopt (argc, argv, "i:")) != -1)
  {
    switch (opt)
    {
    case 'i':
      len = strlen (optarg);
      init_file = calloc (len, sizeof (char));
      strncpy (init_file, optarg, len);
      break;
    }
  }
  return init_file;
}

int
main (int argc, char **argv)
{
  /* *INDENT-OFF* */
  static struct wlc_interface interface = {
    .output = {
      .created = output_created, // Done - Untested
      .destroyed = output_destroyed, // Done - Untested
      .focus = output_focus, // Done - Untested
      .resolution = output_resolution, // Done - Untested
      .render = {
        .pre = output_render_pre, // Done - Untested
        .post = output_render_post // Done - Untested
      }
    },
    .view = {
      .created = view_created, // Done
      .destroyed = view_destroyed, // Done - Untested
      .focus = view_focus, // Done - Untested
      .move_to_output = view_move_to_output, // Done - Untested
      /* punting on these for the moment */
      /* .request = { */
      /*   .geometry = view_request_geometry, */
      /*   .state = view_request_state, */
      /*   .move = view_request_move, */
      /*   .resive = view_request_resize, */
      /* }, */
      .render = {
        .pre = view_render_pre, // Done - Untested
        .post = view_render_post // Done - Untested
      }
    },
    .keyboard = {
      .key = keyboard_key, // Done - should add keyup
    },
    .pointer = {
      /* the .button and .scroll events should be tied into the key
         system e.g. (kbd "M-Mouse1"), (kbd "M-ScrollUp") */
      /* .button = pointer_button,  */
      /* .scroll = pointer_scroll, */
      /* .motion = pointer_motion, // Done - untested */
    },
    /* this .touch should also be tied into the key system */
    /* .touch = { */
    /*   .touch = touch_touch, */
    /* }, */
    .compositor = {
      .ready = compositor_ready,
      .terminate = compositor_terminate,
    },
    /* Experimental -- Don't see need for at the moment */
    /* .input = { */
    /*   .created = input_created, */
    /*   .destroyed = input_destroyed, */
    /* } */
  };
  /* *INDENT-ON* */

  if (!wlc_init (&interface, argc, argv))
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
