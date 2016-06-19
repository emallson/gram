#include <libguile.h>
#include <wlc/wlc.h>

#include "output_focus.h"
#include "../types/output.h"

static SCM gram_output_focus_hook;
static SCM gram_output_focus_hook_object;

void
gram_output_focus_hook_init (void)
{
  gram_output_focus_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (2)));
  gram_output_focus_hook_object =
    scm_permanent_object (scm_c_define
                          ("output-focus-hook", gram_output_focus_hook));
  scm_c_export ("output-focus-hook", NULL);
}

SCM
gram_output_focus_hook_run (void *data)
{
  struct output_focus_input *input = (struct output_focus_input *) data;
  scm_c_run_hook (gram_output_focus_hook,
                  scm_list_2 (gram_output_scm (input->handle),
                              input->focus ? SCM_BOOL_T : SCM_BOOL_F));
  return SCM_UNSPECIFIED;
}
