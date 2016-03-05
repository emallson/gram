#include <libguile.h>
#include <wlc/wlc.h>

#include "output_created.h"
#include "../types/output.h"

static SCM gram_output_created_hook;
static SCM gram_output_created_hook_object;

void
gram_output_created_hook_init (void)
{
  gram_output_created_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (1)));
  gram_output_created_hook_object =
    scm_permanent_object (scm_c_define
                          ("output-created-hook", gram_output_created_hook));
  scm_c_export("output-created-hook", NULL);
}

void *
gram_output_created_hook_run (void *data)
{
  scm_c_run_hook (gram_output_created_hook,
                  scm_list_1 (gram_output_scm(*(const wlc_handle*)data)));
  return SCM_UNSPECIFIED;
}
