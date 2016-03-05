#include <libguile.h>
#include <wlc/wlc.h>

#include "view_destroyed.h"
#include "../types/view.h"

static SCM gram_view_destroyed_hook;
static SCM gram_view_destroyed_hook_object;

void
gram_view_destroyed_hook_init (void)
{
  gram_view_destroyed_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (1)));
  gram_view_destroyed_hook_object =
    scm_permanent_object (scm_c_define
                          ("view-destroyed-hook", gram_view_destroyed_hook));
}

void *
gram_view_destroyed_hook_run (void *data)
{
  scm_c_run_hook (gram_view_destroyed_hook,
                  scm_make_list (scm_from_unsigned_integer (1),
                                 gram_view_scm (*(const wlc_handle *) data)));
  return SCM_UNSPECIFIED;
}
