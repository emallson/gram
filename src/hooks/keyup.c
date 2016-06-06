#include <libguile.h>
#include <wlc/wlc.h>

#include "keyup.h"
#include "../types/keysym.h"
#include "../types/view.h"

static SCM gram_keyup_hook;
static SCM gram_keyup_hook_object;

void
gram_keyup_hook_init (void)
{
  gram_keyup_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (2)));
  gram_keyup_hook_object =
    scm_permanent_object (scm_c_define ("keyup-hook", gram_keyup_hook));
  scm_c_export ("keyup-hook", NULL);
}

void *
gram_keyup_hook_run (void *data)
{
  struct keyup_input* input = (struct keyup_input*) data;
  scm_c_run_hook (gram_keyup_hook,
                  scm_list_2 (gram_keysym_scm (&input->keysym),
                              gram_view_scm(input->view)));
  return (void *) &gram_swallow;
}
