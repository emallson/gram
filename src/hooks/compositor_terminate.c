#include <libguile.h>
#include <wlc/wlc.h>

#include "compositor_terminate.h"

static SCM gram_compositor_terminate_hook;
static SCM gram_compositor_terminate_hook_object;

void
gram_compositor_terminate_hook_init (void)
{
  gram_compositor_terminate_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (0)));
  gram_compositor_terminate_hook_object =
    scm_permanent_object (scm_c_define
                          ("compositor-terminate-hook",
                           gram_compositor_terminate_hook));
  scm_c_export ("compositor-terminate-hook", NULL);
}

SCM
gram_compositor_terminate_hook_run (void *data)
{
  scm_c_run_hook (gram_compositor_terminate_hook, SCM_EOL);
  return SCM_UNSPECIFIED;
}
