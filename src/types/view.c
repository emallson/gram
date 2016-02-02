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

void
gram_view_deactivate (const wlc_handle view)
{
  uint32_t i;
  for (i = 0; i < GRAM_MAX_VIEWS; i++)
  {
    if (view_table[i] && view_table[i]->view == view)
    {
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
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    wlc_view_close (view->view);
  }

  view->active = false;
  return _view;
}

static SCM
gram_view_bring_to_front (SCM _view)
{
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
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    wlc_view_focus (view->view);
  }
  return _view;
}

static SCM
gram_geometry_scm (const struct wlc_geometry *geo)
{
  /* can't make records from c and a new smob for this is really overkill */
  return scm_cons (scm_cons (scm_from_uint32 (geo->origin.x),
                             scm_from_uint32 (geo->origin.y)),
                   scm_cons (scm_from_uint32 (geo->size.w),
                             scm_from_uint32 (geo->size.h)));
}

static SCM
gram_view_get_geometry (SCM _view)
{
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    return gram_geometry_scm (wlc_view_get_geometry (view->view));
  }
  return SCM_BOOL_F;
}

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

static SCM
gram_view_get_state (SCM _view)
{
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    return gram_view_state_scm (wlc_view_get_state (view->view));
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
gram_view_get_output (SCM _view)
{
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
gram_view_get_app_id (SCM _view)
{
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
      return SCM_BOOL_F;
    }
  }
  return SCM_BOOL_F;
}

static SCM
gram_view_get_class (SCM _view)
{
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    return scm_from_locale_string (wlc_view_get_class (view->view));
  }
  return SCM_BOOL_F;
}

static SCM
gram_view_type_scm (uint32_t type)
{
  switch (type)
  {
  case WLC_BIT_MODAL:
    return scm_from_locale_symbol ("modal");
  case WLC_BIT_OVERRIDE_REDIRECT:
    return scm_from_locale_symbol ("override-redirect");
  case WLC_BIT_POPUP:
    return scm_from_locale_string ("popup");
  case WLC_BIT_SPLASH:
    return scm_from_locale_string ("splash");
  case WLC_BIT_UNMANAGED:
    return scm_from_locale_string ("unmanaged");
  }
  printf ("Unknown Type: 0x%x\n", type);
  return SCM_BOOL_F;
}

static SCM
gram_view_get_type (SCM _view)
{
  struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
  if (view->active)
  {
    return gram_view_type_scm (wlc_view_get_type (view->view));
  }
  return SCM_BOOL_F;
}

static void
init_gram_view_methods (void)
{
  scm_c_define_gsubr ("view-close", 1, 0, 0, gram_view_close);
  scm_c_define_gsubr ("view-bring-to-front", 1, 0, 0,
                      gram_view_bring_to_front);
  scm_c_define_gsubr ("view-send-to-back", 1, 0, 0, gram_view_send_to_back);
  scm_c_define_gsubr ("view-focus", 1, 0, 0, gram_view_focus);
  scm_c_define_gsubr ("view-get-geometry", 1, 0, 0, gram_view_get_geometry);
  scm_c_define_gsubr ("view-get-state", 1, 0, 0, gram_view_get_state);
  /* scm_c_define_gsubr ("view-get-mask", 1, 0, 0, gram_view_get_mask); */
  scm_c_define_gsubr ("view-get-output", 1, 0, 0, gram_view_get_output);
  scm_c_define_gsubr ("view-get-app-id", 1, 0, 0, gram_view_get_app_id);
  scm_c_define_gsubr ("view-get-class", 1, 0, 0, gram_view_get_class);
  scm_c_define_gsubr ("view-get-type", 1, 0, 0, gram_view_get_type);
}

void
init_gram_view (void)
{
  gram_view_tag = scm_make_smob_type ("view", sizeof (struct gram_view));
  scm_set_smob_print (gram_view_tag, gram_view_print);
  scm_set_smob_free (gram_view_tag, gram_view_free);

  init_gram_view_methods ();
}
