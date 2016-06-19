#include <libguile.h>
#include <wlc/wlc.h>

#include "output_render_post.h"
#include "../types/output.h"

static SCM gram_output_render_post_hook;
static SCM gram_output_render_post_hook_object;

void
gram_output_render_post_hook_init (void)
{
  gram_output_render_post_hook =
    scm_permanent_object (scm_make_hook (scm_from_unsigned_integer (1)));
  gram_output_render_post_hook_object =
    scm_permanent_object (scm_c_define
                          ("output-render-post-hook",
                           gram_output_render_post_hook));
  scm_c_export ("output-render-post-hook", NULL);
}

SCM
gram_output_render_post_hook_run (void *data)
{

  scm_c_run_hook (gram_output_render_post_hook,
                  scm_list_1 (gram_output_scm (*(const wlc_handle *) data)));
  return SCM_UNSPECIFIED;
}
