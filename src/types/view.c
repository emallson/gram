#include <libguile.h>
#include <wlc/wlc.h>

#include "view.h"
#include "output.h"

static struct gram_view *view_table[GRAM_MAX_VIEWS];
static SCM smob_table[GRAM_MAX_VIEWS];

static int
gram_view_print (SCM view_smob, SCM port, scm_print_state * pstate)
{
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (view_smob);

  scm_puts ("#<view ", port);
  scm_puts (wlc_view_get_title (view->view), port);
  scm_puts (">", port);

  return 1;
}

SCM
gram_view_scm (const wlc_handle view)
{
  uint32_t i;
  for (i = 0; i < GRAM_MAX_VIEWS; i++)
  {
    if (view_table[i] && view_table[i]->view == view)
    {
      break;
    }
  }

  /* view not in table */
  if (i >= GRAM_MAX_VIEWS)
  {
    for (i = 0; i < GRAM_MAX_VIEWS; i++)
    {
      if (view_table[i] == NULL)
      {
        view_table[i] = (struct gram_view *)
          scm_gc_malloc (sizeof (struct gram_view), "view");
        *(wlc_handle *) & view_table[i]->view = view;
        smob_table[i] = scm_new_smob (gram_view_tag,
                                      (scm_t_bits) view_table[i]);
        break;
      }
    }
  }

  if (i >= GRAM_MAX_VIEWS)
  {
    /* still no room */
    return SCM_BOOL_F;
  }
  view_table[i]->active = true;
  return smob_table[i];
}

SCM
gram_view_viewp (SCM maybe_view)
{
  if (SCM_SMOB_PREDICATE (gram_view_tag, maybe_view))
  {
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

SCM
gram_view_activep (SCM _view) {
  if(SCM_SMOB_PREDICATE(gram_view_tag, _view)) {
    struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
    return view->active ? SCM_BOOL_T : SCM_BOOL_F;
  }
  return SCM_BOOL_F;
}

void
gram_view_deactivate (const wlc_handle view)
{
  uint32_t i;
  for (i = 0; i < GRAM_MAX_VIEWS; i++)
  {
    if (view_table[i] && view_table[i]->view == view)
    {
      printf("View %d deactivated\n", i);
      view_table[i]->active = false;
    }
  }
}

static size_t
gram_view_free (SCM _view)
{
  uint32_t i;
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  for (i = 0; i < GRAM_MAX_VIEWS; i++)
  {
    if (view_table[i] == view)
    {
      view_table[i] = NULL;
      smob_table[i] = NULL;
    }
  }

  scm_gc_free (view, sizeof (struct gram_view), "view");
  return 0;
}


static SCM
gram_view_close (SCM _view)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    wlc_view_close (view->view);
  }

  view->active = false;
  return SCM_BOOL_T;
}

static SCM
gram_view_bring_to_front (SCM _view)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    wlc_view_bring_to_front (view->view);
  }
  return _view;
}

static SCM
gram_view_send_to_back (SCM _view)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    wlc_view_send_to_back (view->view);
  }
  return _view;
}

static SCM
gram_view_focus (SCM _view)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    wlc_view_focus (view->view);
  }
  return _view;
}

SCM
gram_geometry_scm (const struct wlc_geometry * geo)
{
  /* can't make records from c and a new smob for this is really overkill */
  return scm_cons (scm_cons (scm_from_uint32 (geo->origin.x),
                             scm_from_uint32 (geo->origin.y)),
                   scm_cons (scm_from_uint32 (geo->size.w),
                             scm_from_uint32 (geo->size.h)));
}

/* converts an SCM to a wlc_geometry. Assumes input is valid. */
static const struct wlc_geometry
gram_geometry_from_scm (SCM _geo)
{
  struct wlc_geometry geo = {
    {scm_to_uint32 (scm_caar (_geo)),
     scm_to_uint32 (scm_cdar (_geo))},
    {scm_to_uint32 (scm_cadr (_geo)),
     scm_to_uint32 (scm_cddr (_geo))}
  };

  return geo;
}

static SCM
gram_view_get_geometry (SCM _view)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    return gram_geometry_scm (wlc_view_get_geometry (view->view));
  }
  return SCM_BOOL_F;
}

static SCM
gram_view_set_geometry (SCM _view, SCM _geo)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    if (scm_pair_p (_geo) && scm_pair_p (scm_car (_geo))
        && scm_pair_p (scm_cdr (_geo)))
    {
      const struct wlc_geometry geo = gram_geometry_from_scm (_geo);
      const struct wlc_geometry curr = *wlc_view_get_geometry(view->view);
      if(curr.origin.x == geo.origin.x &&
        curr.origin.y == geo.origin.y &&
        curr.size.w == geo.size.w &&
         curr.size.h == geo.size.h) {
        /* no change, do nothing */
        return _view;
      }
      wlc_view_set_geometry (view->view, 0, &geo);
      printf ("Set %s to (%d, %d)\n", wlc_view_get_title (view->view),
              geo.size.w, geo.size.h);
      return _view;
    }
  }
  return SCM_BOOL_F;
}

/* not sure this is right. seems like a bitmask? */
static SCM
gram_view_state_scm (uint32_t state)
{
  switch (state)
  {
  case WLC_BIT_ACTIVATED:
    return scm_from_locale_symbol ("activated");
  case WLC_BIT_FULLSCREEN:
    return scm_from_locale_symbol ("fullscreen");
  case WLC_BIT_MAXIMIZED:
    return scm_from_locale_symbol ("maximized");
  case WLC_BIT_MOVING:
    return scm_from_locale_string ("moving");
  case WLC_BIT_RESIZING:
    return scm_from_locale_string ("resizing");
  }
  return SCM_BOOL_F;
}

/* see above comment */
static uint32_t
gram_view_state_from_scm (SCM _state)
{
  if (scm_eq_p (scm_from_locale_symbol ("activated"), _state))
  {
    return WLC_BIT_ACTIVATED;
  }
  if (scm_eq_p (scm_from_locale_symbol ("fullscreen"), _state))
  {
    return WLC_BIT_FULLSCREEN;
  }
  if (scm_eq_p (scm_from_locale_symbol ("maximized"), _state))
  {
    return WLC_BIT_MAXIMIZED;
  }
  if (scm_eq_p (scm_from_locale_symbol ("moving"), _state))
  {
    return WLC_BIT_MOVING;
  }
  if (scm_eq_p (scm_from_locale_symbol ("resizing"), _state))
  {
    return WLC_BIT_RESIZING;
  }
  return -1;
}

static SCM
gram_view_get_state (SCM _view)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    return gram_view_state_scm (wlc_view_get_state (view->view));
  }
  return SCM_BOOL_F;
}


static SCM
gram_view_set_state (SCM _view, SCM _state, SCM _toggle)
{
  scm_assert_smob_type (gram_view_tag, _view);

  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    uint32_t state = gram_view_state_from_scm (_state);
    if (state != -1)
    {
      wlc_view_set_state (view->view, state, scm_is_false_or_nil (_toggle));
      return SCM_ELISP_NIL;
    }
    return SCM_BOOL_F;
  }
  return SCM_BOOL_F;
}

/* not sure what this is supposed to be interpreted as; punting */
/* static SCM */
/* gram_view_get_mask (SCM _view) */
/* { */
/*   struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view); */
/*   if (view->active) */
/*     { */
/*       return gram_view_mask_scm(wlc_view_get_mask (view->view)); */
/*     } */
/*   return SCM_BOOL_F; */
/* } */

static SCM
gram_view_get_parent (SCM _view)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    return gram_view_scm (wlc_view_get_parent (view->view));
  }
  return SCM_BOOL_F;
}

static SCM
gram_view_set_parent (SCM _view, SCM _parent)
{
  scm_assert_smob_type (gram_view_tag, _view);
  scm_assert_smob_type (gram_view_tag, _parent);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  struct gram_view *parent = (struct gram_view *) SCM_SMOB_DATA (_parent);

  if (view->active && parent->active)
  {
    wlc_view_set_parent (view->view, parent->view);
    return SCM_ELISP_NIL;
  }
  return SCM_BOOL_F;
}

static SCM
gram_view_get_output (SCM _view)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    const wlc_handle out = wlc_view_get_output (view->view);
    SCM out_smob = gram_output_scm (out);
    return out_smob;
  }
  printf ("Inactive view accessed: %lu\n", view->view);
  return SCM_BOOL_F;
}

static SCM
gram_view_set_output (SCM _view, SCM _output)
{
  scm_assert_smob_type (gram_view_tag, _view);
  scm_assert_smob_type (gram_output_tag, _output);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  struct gram_output *output = (struct gram_output *) SCM_SMOB_DATA (_output);

  if (view->active && output->active)
  {
    wlc_view_set_output (view->view, output->output);
    printf ("Set output of %s\n", wlc_view_get_title (view->view));
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static SCM
gram_view_get_app_id (SCM _view)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    const char *app_id = wlc_view_get_app_id (view->view);
    if (app_id)
    {
      return scm_from_locale_string (app_id);
    }
    else
    {
      return scm_from_locale_string("");
    }
  }
  return SCM_BOOL_F;
}

static SCM
gram_view_get_class (SCM _view)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    const char* class = wlc_view_get_class(view->view);
    if(class != NULL) {
      return scm_from_locale_string(class);
    } else {
      return scm_from_locale_string("");
    }
  }
  return SCM_BOOL_F;
}

static SCM
gram_view_get_title (SCM _view)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    const char* title = wlc_view_get_title(view->view);
    if(title != NULL) {
      return scm_from_locale_string(title);
    } else {
      return scm_from_locale_string("");
    }
  }
  return SCM_BOOL_F;
}

static SCM
gram_view_type_scm (uint32_t type)
{
  SCM types = SCM_EOL;
  if(type & WLC_BIT_MODAL) {
    types = scm_cons(scm_from_locale_symbol("modal"), types);
  }

  if(type & WLC_BIT_OVERRIDE_REDIRECT) {
    types = scm_cons(scm_from_locale_symbol("override-redirect"), types);
  }

  if(type & WLC_BIT_POPUP) {
    types = scm_cons(scm_from_locale_symbol("popup"), types);
  }

  if(type & WLC_BIT_SPLASH) {
    types = scm_cons(scm_from_locale_symbol("splash"), types);
  }

  if(type & WLC_BIT_UNMANAGED) {
    types = scm_cons(scm_from_locale_symbol("unmanaged"), types);
  }

  return types;
}

static SCM
gram_view_get_types (SCM _view)
{
  scm_assert_smob_type (gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    return gram_view_type_scm (wlc_view_get_type (view->view));
  }
  return SCM_BOOL_F;
}

static SCM
gram_view_show (SCM _view, SCM _output)
{
  scm_assert_smob_type(gram_view_tag, _view);
  scm_assert_smob_type(gram_output_tag, _output);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA(_view);
  struct gram_output *output = (struct gram_output *) SCM_SMOB_DATA(_output);
  if(view->active && output->active) {
    wlc_view_set_mask (view->view,
                       wlc_output_get_mask (wlc_view_get_output
                                            (view->view)));
    return _view;
  }
  return SCM_BOOL_F;
}

static SCM
gram_view_hide (SCM _view)
{
  scm_assert_smob_type(gram_view_tag, _view);
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA(_view);
  if(view->active) {
    wlc_view_set_mask(view->view, 0);
    return _view;
  }
  return SCM_BOOL_F;
}

static void
init_gram_view_methods (void *data)
{
  scm_c_define_gsubr ("close", 1, 0, 0, gram_view_close);
  scm_c_define_gsubr ("bring-to-front", 1, 0, 0, gram_view_bring_to_front);
  scm_c_define_gsubr ("send-to-back", 1, 0, 0, gram_view_send_to_back);
  scm_c_define_gsubr ("focus", 1, 0, 0, gram_view_focus);
  scm_c_define_gsubr ("get-geometry", 1, 0, 0, gram_view_get_geometry);
  scm_c_define_gsubr ("set-geometry", 2, 0, 0, gram_view_set_geometry);
  scm_c_define_gsubr ("get-state", 1, 0, 0, gram_view_get_state);
  scm_c_define_gsubr ("set-state", 3, 0, 0, gram_view_set_state);
  /* scm_c_define_gsubr ("get-mask", 1, 0, 0, gram_view_get_mask); */
  scm_c_define_gsubr ("get-parent", 1, 0, 0, gram_view_get_parent);
  scm_c_define_gsubr ("set-parent", 2, 0, 0, gram_view_set_parent);
  scm_c_define_gsubr ("get-output", 1, 0, 0, gram_view_get_output);
  scm_c_define_gsubr ("set-output", 2, 0, 0, gram_view_set_output);
  scm_c_define_gsubr ("get-app-id", 1, 0, 0, gram_view_get_app_id);
  scm_c_define_gsubr ("get-class", 1, 0, 0, gram_view_get_class);
  scm_c_define_gsubr ("get-title", 1, 0, 0, gram_view_get_title);
  scm_c_define_gsubr ("get-types", 1, 0, 0, gram_view_get_types);
  scm_c_define_gsubr ("view?", 1, 0, 0, gram_view_viewp);
  scm_c_define_gsubr ("active?", 1, 0, 0, gram_view_activep);
  scm_c_define_gsubr ("hide", 1, 0, 0, gram_view_hide);
  scm_c_define_gsubr ("show", 2, 0, 0, gram_view_show);

  scm_c_export ("close", "bring-to-front", "send-to-back", "focus",
                "get-geometry", "set-geometry",
                "get-state", "set-state",
                "get-parent", "set-parent",
                "get-output", "set-output",
                "get-app-id", "get-class", "get-title",
                "get-types", "view?", "active?", "hide", "show", NULL);
}

void
init_gram_view (void)
{
  for (uint32_t i = 0; i < GRAM_MAX_VIEWS; i++)
  {
    view_table[i] = NULL;
    smob_table[i] = NULL;
  }

  gram_view_tag = scm_make_smob_type ("view", sizeof (struct gram_view));
  scm_set_smob_print (gram_view_tag, gram_view_print);
  scm_set_smob_free (gram_view_tag, gram_view_free);

  scm_c_define_module ("gram view", init_gram_view_methods, NULL);
}
