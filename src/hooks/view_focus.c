#include <libguile.h>
#include <wlc/wlc.h>

#include "view_focus.h"
#include "../types/view.h"

static SCM gram_view_focus_hook;
static SCM gram_view_focus_hook_object;

void
gram_view_focus_hook_init (void)
{
  gram_view_focus_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (2)));
  gram_view_focus_hook_object =
    scm_permanent_object (scm_c_define
                          ("view-focus-hook", gram_view_focus_hook));
  scm_c_export ("view-focus-hook", NULL);
}

void *
gram_view_focus_hook_run (void *data)
{
  struct view_focus_input *input = (struct view_focus_input *) data;
  scm_c_run_hook (gram_view_focus_hook,
                  scm_list_2 (gram_view_scm (input->handle),
                              input->focus ? SCM_BOOL_T : SCM_BOOL_F));
  return SCM_UNSPECIFIED;
}
