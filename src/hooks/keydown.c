#include <libguile.h>
#include <wlc/wlc.h>

#include "keydown.h"
#include "../types/keysym.h"

static SCM gram_keydown_hook;
static SCM gram_keydown_hook_object;

void
gram_keydown_hook_init (void)
{
  gram_keydown_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (1)));
  gram_keydown_hook_object =
    scm_permanent_object (scm_c_define ("keydown-hook", gram_keydown_hook));
}

void *
gram_keydown_hook_run (void *data)
{
  scm_c_run_hook (gram_keydown_hook,
                  scm_make_list (scm_from_unsigned_integer (1),
                                 gram_keysym_scm ((struct gram_keysym *)
                                                  data)));
  return (void *) &gram_swallow;
}