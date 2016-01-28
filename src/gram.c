#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <libguile.h>
#include <wlc/wlc.h>

#include "types/keysym.h"

static void
init_gram_types (void)
{
  init_gram_keysym ();
}

static SCM gram_keydown_hook;
static SCM gram_keydown_hook_object;
static void
init_gram_hooks (void)
{
  gram_keydown_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (1)));
  gram_keydown_hook_object =
    scm_permanent_object (scm_c_define ("*keydown-hook*", gram_keydown_hook));
}

static void *
gram_keydown_hook_run (void *data)
{
  scm_c_run_hook (gram_keydown_hook,
		  scm_make_list (scm_from_unsigned_integer (1),
				 gram_keysym_scm ((struct gram_keysym *)
						  data)));
}

static bool
keyboard_key_is_mod (uint32_t key)
{
  switch (key)
    {
    case XKB_KEY_Control_L:
    case XKB_KEY_Control_R:
    case XKB_KEY_Alt_L:
    case XKB_KEY_Alt_R:
    case XKB_KEY_Super_L:
    case XKB_KEY_Super_R:
    case XKB_KEY_Shift_L:
    case XKB_KEY_Shift_R:
      return true;
    default:
      return false;
    }
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
    .mods = mods
  };

  if (state == WLC_KEY_STATE_PRESSED)
    {
      scm_with_guile (gram_keydown_hook_run, &keysym);
    }

  return true;
}

static bool
view_created (wlc_handle view)
{
  wlc_view_bring_to_front (view);
  wlc_view_focus (view);
  return true;
}

static void
view_focus (wlc_handle view, bool focus)
{
  wlc_view_set_state (view, WLC_BIT_ACTIVATED, focus);
}

static void
load_init (void *data)
{
  scm_c_primitive_load ((char *) data);
}

static void *
init_guile (void *data)
{
  init_gram_types ();
  init_gram_hooks ();

  if(data != NULL)
    {
      load_init (data);
    }
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
  static struct wlc_interface interface = {
    .view = {
	     .created = view_created,
	     .focus = view_focus,
	     },
    .keyboard = {
		 .key = keyboard_key,
		 },
  };

  if (!wlc_init (&interface, argc, argv))
    return EXIT_FAILURE;

  char *init_file = get_init_file (argc, argv);
  if (access (init_file, F_OK|R_OK) == -1)
    {
      init_file = NULL;
    }
  scm_with_guile (init_guile, init_file);

  wlc_run ();
  return EXIT_SUCCESS;
}
