#include <libguile.h>
#include <wlc/wlc.h>

#include "view_request_geometry.h"
#include "../types/view.h"

static SCM gram_view_request_geometry_hook;
static SCM gram_view_request_geometry_hook_object;

void
gram_view_request_geometry_hook_init (void)
{
  gram_view_request_geometry_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (2)));
  gram_view_request_geometry_hook_object =
    scm_permanent_object (scm_c_define
                          ("view-request-geometry-hook",
                           gram_view_request_geometry_hook));
  scm_c_export ("view-request-geometry-hook", NULL);
}

SCM
gram_view_request_geometry_hook_run (void *data)
{
  struct view_geo *s = (struct view_geo *) data;
  scm_c_run_hook (gram_view_request_geometry_hook,
                  scm_list_2 (gram_view_scm (s->view),
                              gram_geometry_scm (s->geo)));
  return SCM_BOOL_T;
}
