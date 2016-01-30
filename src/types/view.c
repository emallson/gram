#include <libguile.h>
#include <wlc/wlc.h>

#include "view.h"

static SCM
gram_view_equalp(SCM a, SCM b)
{
  struct gram_view *k_a = (struct gram_view *) SCM_SMOB_DATA (a);
  struct gram_view *k_b = (struct gram_view *) SCM_SMOB_DATA (b);

  if (k_a->view == k_b->view) {
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static int
gram_view_print(SCM view_smob, SCM port, scm_print_state* pstate)
{
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (view_smob);

  scm_puts("#<view ", port);
  scm_puts(wlc_view_get_title(view->view), port);
  scm_puts(">", port);

  return 1;
}

SCM gram_view_scm(struct gram_view *_view)
{
  /* honestly not sure what I need to do to safely give access to a
     view in guile. */
  struct gram_view *view = (struct gram_view *)
    scm_gc_malloc (sizeof (struct gram_view), "view");

  memcpy (view, _view, sizeof(struct gram_view));

  return scm_new_smob (gram_view_tag, (scm_t_bits) view);
}

/* Please don't hate me, this is a valid macro use IMO */
#define GRAM_VIEW_WRAP(FN) static SCM           \
  gram_view_##FN (SCM _view) { \
    struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA(_view); \
    wlc_view_##FN(view->view); \
    return _view; \
  }

/* no setters yet */
GRAM_VIEW_WRAP(close);
GRAM_VIEW_WRAP(bring_to_front);
GRAM_VIEW_WRAP(send_to_back);
GRAM_VIEW_WRAP(focus);
GRAM_VIEW_WRAP(get_geometry);
GRAM_VIEW_WRAP(get_state);
GRAM_VIEW_WRAP(get_mask);
GRAM_VIEW_WRAP(get_output);
GRAM_VIEW_WRAP(get_app_id);
GRAM_VIEW_WRAP(get_class);
GRAM_VIEW_WRAP(get_type);

static void
init_gram_view_methods( void )
{
#define GRAM_VIEW_GSUBR(NAME, FN) \
  gram_view_##FN##_tag = scm_c_define_gsubr(NAME, 1, 0, 0, gram_view_##FN);

  GRAM_VIEW_GSUBR("view-close", close);
  GRAM_VIEW_GSUBR("view-bring-to-front", bring_to_front);
  GRAM_VIEW_GSUBR("view-send-to-back", send_to_back);
  GRAM_VIEW_GSUBR("view-focus", focus);
  GRAM_VIEW_GSUBR("view-get-geometry", get_geometry);
  GRAM_VIEW_GSUBR("view-get-state", get_state);
  GRAM_VIEW_GSUBR("view-get-mask", get_mask);
  GRAM_VIEW_GSUBR("view-get-output", get_output);
  GRAM_VIEW_GSUBR("view-get-app-id", get_app_id);
  GRAM_VIEW_GSUBR("view-get-class", get_class);
  GRAM_VIEW_GSUBR("view-get-type", get_type);
}

void
init_gram_view(void)
{
  gram_view_tag =
    scm_make_smob_type("view", sizeof(struct gram_view));
  scm_set_smob_print(gram_view_tag, gram_view_print);
  scm_set_smob_equalp(gram_view_tag, gram_view_equalp);

  init_gram_view_methods();
}
