#include <libguile.h>
#include <wlc/wlc.h>

#include "keydown.h"
#include "../types/keysym.h"
#include "../types/view.h"

static SCM gram_keydown_hook;
static SCM gram_keydown_hook_object;

void
gram_keydown_hook_init (void)
{
  gram_keydown_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (2)));
  gram_keydown_hook_object =
    scm_permanent_object (scm_c_define ("keydown-hook", gram_keydown_hook));
  scm_c_export ("keydown-hook", NULL);
}

void *
gram_keydown_hook_run (void *data)
{
  struct keydown_input* input = (struct keydown_input*) data;
  scm_c_run_hook (gram_keydown_hook,
                  scm_list_2 (gram_keysym_scm (&input->keysym),
                              gram_view_scm(input->view)));
  return (void *) &gram_swallow;
}
