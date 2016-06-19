#include <libguile.h>
#include <wlc/wlc.h>

#include "output_resolution.h"
#include "../types/output.h"

static SCM gram_output_resolution_hook;
static SCM gram_output_resolution_hook_object;

void
gram_output_resolution_hook_init (void)
{
  gram_output_resolution_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (3)));
  gram_output_resolution_hook_object =
    scm_permanent_object (scm_c_define
                          ("output-resolution-hook",
                           gram_output_resolution_hook));
  scm_c_export ("output-resolution-hook", NULL);
}

SCM
gram_output_resolution_hook_run (void *data)
{
  struct resolution_input *input = (struct resolution_input *) data;
  scm_c_run_hook (gram_output_resolution_hook,
                  scm_list_3 (gram_output_scm (input->handle),
                              scm_cons (scm_from_uint32 (input->from->w),
                                        scm_from_uint32 (input->from->h)),
                              scm_cons (scm_from_uint32 (input->to->w),
                                        scm_from_uint32 (input->to->h))));
  return SCM_UNSPECIFIED;
}
