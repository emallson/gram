#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>

#include <libguile.h>
#include <wlc/wlc.h>

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
  gram_view_deactivate (view);
}

static void
view_focus (wlc_handle view, bool focus)
{
  wlc_view_set_state (view, WLC_BIT_ACTIVATED, focus);
}

static void *
load_init (void *data)
{
  scm_c_primitive_load ((char *) data);
  return SCM_UNSPECIFIED;
}

static void *
init_guile (void *data)
{
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
    .view = {
      .created = view_created,
      .focus = view_focus,
      .destroyed = view_destroyed,
    },
    .keyboard = {
      .key = keyboard_key,
    },
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
