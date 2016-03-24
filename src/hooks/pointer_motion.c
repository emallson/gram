#include <libguile.h>
#include <wlc/wlc.h>

#include "pointer_motion.h"
#include "../types/view.h"

static SCM gram_pointer_motion_hook;
static SCM gram_pointer_motion_hook_object;

void
gram_pointer_motion_hook_init (void)
{
  gram_pointer_motion_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (2)));
  gram_pointer_motion_hook_object =
    scm_permanent_object (scm_c_define
                          ("pointer-motion-hook", gram_pointer_motion_hook));
  scm_c_export ("pointer-motion-hook", NULL);
}

void *
gram_pointer_motion_hook_run (void *data)
{
  struct pointer_motion_input *input = (struct pointer_motion_input *) data;
  scm_c_run_hook (gram_pointer_motion_hook,
                  scm_list_2 (gram_view_scm (input->view),
                              scm_cons (scm_from_uint32 (input->point->x),
                                        scm_from_uint32 (input->point->y))));
  return SCM_UNSPECIFIED;
}
