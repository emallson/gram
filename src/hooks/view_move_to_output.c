#include <libguile.h>
#include <wlc/wlc.h>

#include "view_move_to_output.h"
#include "../types/view.h"
#include "../types/output.h"

static SCM gram_view_move_to_output_hook;
static SCM gram_view_move_to_output_hook_object;

void
gram_view_move_to_output_hook_init (void)
{
  gram_view_move_to_output_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (3)));
  gram_view_move_to_output_hook_object =
    scm_permanent_object (scm_c_define
                          ("view-move-to-output-hook",
                           gram_view_move_to_output_hook));
  scm_c_export ("view-move-to-output-hook", NULL);
}

SCM
gram_view_move_to_output_hook_run (void *data)
{
  struct move_to_output_input *input = (struct move_to_output_input *) data;
  scm_c_run_hook (gram_view_move_to_output_hook,
                  scm_list_3 (gram_view_scm (input->view),
                              gram_output_scm (input->from_out),
                              gram_output_scm (input->to_out)));
  return SCM_UNSPECIFIED;
}
