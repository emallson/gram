#include <libguile.h>
#include <wlc/wlc.h>

#include "output_destroyed.h"
#include "../types/output.h"

static SCM gram_output_destroyed_hook;
static SCM gram_output_destroyed_hook_object;

void
gram_output_destroyed_hook_init (void)
{
  gram_output_destroyed_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (1)));
  gram_output_destroyed_hook_object =
    scm_permanent_object (scm_c_define
                          ("output-destroyed-hook",
                           gram_output_destroyed_hook));
  scm_c_export ("output-destroyed-hook", NULL);
}

SCM
gram_output_destroyed_hook_run (void *data)
{
  scm_c_run_hook (gram_output_destroyed_hook,
                  scm_list_1 (gram_output_scm (*(const wlc_handle *) data)));
  return SCM_UNSPECIFIED;
}
