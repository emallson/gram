#include <libguile.h>
#include <wlc/wlc.h>

#include "compositor_ready.h"

static SCM gram_compositor_ready_hook;
static SCM gram_compositor_ready_hook_object;

void
gram_compositor_ready_hook_init (void)
{
  gram_compositor_ready_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (0)));
  gram_compositor_ready_hook_object =
    scm_permanent_object (scm_c_define
                          ("compositor-ready-hook",
                           gram_compositor_ready_hook));
  scm_c_export ("compositor-ready-hook", NULL);
}

SCM
gram_compositor_ready_hook_run (void *data)
{
  scm_c_run_hook (gram_compositor_ready_hook, SCM_EOL);
  return SCM_UNSPECIFIED;
}
