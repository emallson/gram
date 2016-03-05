#include <libguile.h>
#include <wlc/wlc.h>
#include <xkbcommon/xkbcommon.h>

#include "keysym.h"

bool gram_swallow = false;

static SCM
gram_keysym_equalp (SCM a, SCM b)
{
  struct gram_keysym *k_a = (struct gram_keysym *) SCM_SMOB_DATA (a);
  struct gram_keysym *k_b = (struct gram_keysym *) SCM_SMOB_DATA (b);

  if (k_a->sym == k_b->sym && k_a->mods.mods == k_b->mods.mods)
  {
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static int
gram_keysym_print (SCM keysym_smob, SCM port, scm_print_state * pstate)
{
  struct gram_keysym *keysym =
    (struct gram_keysym *) SCM_SMOB_DATA (keysym_smob);

  scm_puts ("#<keysym ", port);
  if (keysym->mods.mods & WLC_BIT_MOD_LOGO)
  {
    scm_puts ("S-", port);
  }
  if (keysym->mods.mods & WLC_BIT_MOD_CTRL)
  {
    scm_puts ("C-", port);
  }
  if (keysym->mods.mods & WLC_BIT_MOD_ALT)
  {
    scm_puts ("M-", port);
  }

  char buf[64];
  xkb_keysym_to_utf8 (keysym->sym, buf, 64);

  if (buf[0] > 0 && buf[0] <= 0x7F)
  {
    xkb_keysym_get_name (keysym->sym, buf, 64);
  }

  SCM name = scm_from_utf8_string (buf);
  scm_display (name, port);
  scm_puts (">", port);

  return 1;
}

SCM
gram_keysym_scm (struct gram_keysym * _keysym)
{
  struct gram_keysym *keysym = (struct gram_keysym *)
    scm_gc_malloc (sizeof (struct gram_keysym), "keysym");

  memcpy (keysym, _keysym, sizeof (struct gram_keysym));

  return scm_new_smob (gram_keysym_tag, (scm_t_bits) keysym);
}

SCM
gram_keysym_construct (SCM key_desc)
{
  char *desc = scm_to_locale_string (key_desc);
  char *buf, *prev = NULL;

  struct gram_keysym keysym;
  keysym.mods.mods = 0;
  keysym.mods.leds = 0;

  buf = strtok (desc, "-<>");
  while (buf != NULL)
  {
    if (prev != NULL && strlen (prev) == 1)
    {
      switch (prev[0])
      {
      case 'S':
        keysym.mods.mods |= WLC_BIT_MOD_MOD2;
        break;
      case 'C':
        keysym.mods.mods |= WLC_BIT_MOD_CTRL;
        break;
      case 'M':
        keysym.mods.mods |= WLC_BIT_MOD_ALT;
        break;
      default:
        /* invalid mod */
        scm_misc_error ("kbd", "~A is not a valid modifier",
                        scm_list_1 (scm_from_locale_string (prev)));
        return SCM_BOOL_F;
      }
    }
    else if (prev != NULL && strlen (prev) == 1)
    {
      scm_misc_error ("kbd", "~A is not a valid keysym",
                      scm_list_1 (key_desc));
      return SCM_BOOL_F;
    }
    keysym.sym = xkb_keysym_from_name (buf, XKB_KEYSYM_CASE_INSENSITIVE);
    prev = buf;
    buf = strtok (NULL, "<->");
  }

  if (keysym.sym == XKB_KEY_NoSymbol)
  {
    scm_misc_error ("kbd", "~A is not a valid keysym",
                    scm_list_1 (scm_from_locale_string (prev)));
    return SCM_BOOL_F;
  }

  return gram_keysym_scm (&keysym);
}

SCM
gram_key_swallow_next (void)
{
  gram_swallow = true;
  return SCM_BOOL_T;
}

void
init_gram_keysym_fns (void *data)
{
  scm_c_define_gsubr ("swallow-next-key", 0, 0, 0, gram_key_swallow_next);
  scm_c_define_gsubr ("kbd", 1, 0, 0, gram_keysym_construct);

  scm_c_export ("swallow-next-key", "kbd", NULL);
}

void
init_gram_keysym (void)
{
  gram_keysym_tag =
    scm_make_smob_type ("keysym", sizeof (struct gram_keysym));
  scm_set_smob_print (gram_keysym_tag, gram_keysym_print);
  scm_set_smob_equalp (gram_keysym_tag, gram_keysym_equalp);

  scm_c_define_module ("gram keysym", init_gram_keysym_fns, NULL);
}
