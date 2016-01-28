#include <libguile.h>
#include <wlc/wlc.h>

#include "keysym.h"

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
  if (keysym->mods.mods & WLC_BIT_MOD_MOD2)
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
  scm_putc (wlc_keyboard_get_utf32_for_key (keysym->keycode, NULL), port);
  scm_puts (">", port);

  return 1;
}

SCM
gram_keysym_scm (struct gram_keysym *_keysym)
{
  struct gram_keysym *keysym = (struct gram_keysym *)
    scm_gc_malloc (sizeof (struct gram_keysym), "keysym");

  memcpy (keysym, _keysym, sizeof (struct gram_keysym));

  return scm_new_smob (gram_keysym_tag, (scm_t_bits) keysym);
}

void
init_gram_keysym (void)
{
  gram_keysym_tag =
    scm_make_smob_type ("keysym", sizeof (struct gram_keysym));
  scm_set_smob_print (gram_keysym_tag, gram_keysym_print);
  scm_set_smob_equalp (gram_keysym_tag, gram_keysym_equalp);
}
