#include <libguile.h>
#include <wlc/wlc.h>
#include <wlc/wlc-render.h>

#include "output.h"
#include "view.h"

static struct gram_output *output_table[GRAM_MAX_OUTPUTS];
static SCM smob_table[GRAM_MAX_VIEWS];

static int
gram_output_print (SCM output_smob, SCM port, scm_print_state * pstate)
{
  struct gram_output *output =
    (struct gram_output *) SCM_SMOB_DATA (output_smob);

  const char* name = wlc_output_get_name (output->output);
  scm_puts ("#<output ", port);
  scm_puts (name? name : "UNTITLED", port);
  scm_puts (">", port);

  return 1;
}

SCM
gram_output_scm (const wlc_handle output)
{
  uint32_t i;
  for (i = 0; i < GRAM_MAX_OUTPUTS; i++)
  {
    if (output_table[i] && output_table[i]->output == output)
    {
      break;
    }
  }

  /* output not in table */
  if (i >= GRAM_MAX_OUTPUTS)
  {
    for (i = 0; i < GRAM_MAX_OUTPUTS; i++)
    {
      if (output_table[i] == NULL)
      {
        output_table[i] = (struct gram_output *)
          scm_gc_malloc_pointerless (sizeof (struct gram_output), "output");

        *(wlc_handle *) & output_table[i]->output = output;
        smob_table[i] =
          scm_new_smob (gram_output_tag, (scm_t_bits) output_table[i]);
        break;
      }
    }
  }

  if (i >= GRAM_MAX_OUTPUTS)
  {
    /* still no room */
    return SCM_ELISP_NIL;
  }
  output_table[i]->active = true;
  return smob_table[i];
}

/* Marks the table entry corresponding to `output` as invalid if it
   exists. */
void
gram_output_deactivate (const wlc_handle output)
{
  uint32_t i;
  for (i = 0; i < GRAM_MAX_OUTPUTS; i++)
  {
    if (output_table[i] && output_table[i]->output == output)
    {
      output_table[i]->active = false;
    }
  }
}

/* Removes the table entry corresponding to `output`. */
static size_t
gram_output_free (SCM _output)
{
  uint32_t i;
  struct gram_output *output = (struct gram_output *) SCM_SMOB_DATA (_output);
  for (i = 0; i < GRAM_MAX_OUTPUTS; i++)
  {
    if (output_table[i] == output)
    {
      output_table[i] = NULL;
      smob_table[i] = NULL;
    }
  }

  scm_gc_free (output, sizeof (struct gram_output), "output");
  return 0;
}

SCM
gram_output_focus (SCM _output)
{
  scm_assert_smob_type (gram_output_tag, _output);
  struct gram_output *output = (struct gram_output *) SCM_SMOB_DATA (_output);
  if (output->active)
  {
    wlc_output_focus (output->output);
    return _output;
  }
  return SCM_ELISP_NIL;
}

SCM
gram_output_get_name (SCM _output)
{
  scm_assert_smob_type (gram_output_tag, _output);
  struct gram_output *output = (struct gram_output *) SCM_SMOB_DATA (_output);
  if (output->active)
  {
    return scm_from_locale_string (wlc_output_get_name (output->output));
  }
  return SCM_ELISP_NIL;
}

SCM
gram_output_get_views (SCM _output)
{
  scm_assert_smob_type (gram_output_tag, _output);
  struct gram_output *output = (struct gram_output *) SCM_SMOB_DATA (_output);
  if (output->active)
  {
    size_t num_views;
    const wlc_handle *views =
      wlc_output_get_views (output->output, &num_views);
    SCM arr = scm_make_array (SCM_UNSPECIFIED,
                              scm_list_1 (scm_from_uint32 (num_views)));

    scm_t_array_handle handle;
    scm_array_get_handle (arr, &handle);
    SCM *els = scm_array_handle_writable_elements (&handle);

    for (size_t i = 0; i < num_views; i++)
    {
      els[i] = gram_view_scm (views[i]);
    }

    scm_array_handle_release (&handle);

    return arr;
  }
  return SCM_ELISP_NIL;
}

SCM
gram_output_get_resolution (SCM _output)
{
  scm_assert_smob_type (gram_output_tag, _output);
  struct gram_output *output = (struct gram_output *) SCM_SMOB_DATA (_output);
  if (output->active)
  {
    const struct wlc_size *size = wlc_output_get_resolution (output->output);
    if(!size) {
      return SCM_BOOL_F;
    }
    return scm_cons (scm_from_uint32 (size->w), scm_from_uint32 (size->h));
  }
  return SCM_ELISP_NIL;
}

SCM
gram_output_get_sleep (SCM _output)
{
  scm_assert_smob_type (gram_output_tag, _output);
  struct gram_output *output = (struct gram_output *) SCM_SMOB_DATA (_output);
  if (output->active)
  {
    return scm_from_bool (wlc_output_get_sleep (output->output));
  }
  return SCM_ELISP_NIL;
}

SCM
gram_output_set_views (SCM _output, SCM _views)
{
  scm_assert_smob_type (gram_output_tag, _output);
  if (!scm_list_p (_views))
    return SCM_BOOL_F;

  struct gram_output *output = (struct gram_output *) SCM_SMOB_DATA (_output);
  if (output->active)
  {
    /* somewhat complex code. we have to loop over the input view list
     * and extract the wlc_handles. we should ignore any inactive
     * input views. unsure if I need to free *views or not */
    size_t num_views = scm_to_uint64 (scm_length (_views));
    wlc_handle *views = calloc (sizeof (wlc_handle), num_views);
    SCM cur = _views;
    size_t i = 0;
    while (!scm_null_p (cur))
    {
      SCM _view = scm_car (cur);
      _views = scm_cdr(cur);
      scm_assert_smob_type (gram_view_tag, _view);
      struct gram_view *view = (struct gram_view *) SCM_SMOB_DATA (_view);
      if (view->active)
      {
        views[i] = view->view;
        i++;
      }
      else
      {
        /* no view, reduce number of views */
        num_views--;
      }
    }
    wlc_output_set_views (output->output, views, num_views);
    free (views);
    return _output;
  }
  return SCM_BOOL_F;
}

SCM
gram_output_set_resolution (SCM _output, SCM _res)
{
  scm_assert_smob_type (gram_output_tag, _output);
  if (!scm_pair_p (_res))
    return SCM_ELISP_NIL;

  struct gram_output *output = (struct gram_output *) SCM_SMOB_DATA (_output);
  if (output->active)
  {
    struct wlc_size res;
    res.w = scm_to_uint32 (scm_car (_res));
    res.h = scm_to_uint32 (scm_cdr (_res));
    wlc_output_set_resolution (output->output, &res);
    return _output;
  }
  return SCM_ELISP_NIL;
}

SCM
gram_output_set_sleep (SCM _output, SCM _sleep)
{
  scm_assert_smob_type (gram_output_tag, _output);
  if (!scm_boolean_p (_sleep))
    return SCM_ELISP_NIL;

  struct gram_output *output = (struct gram_output *) SCM_SMOB_DATA (_output);
  if (output->active)
  {
    wlc_output_set_sleep (output->output, scm_to_bool (_sleep));
    return _output;
  }
  return SCM_ELISP_NIL;
}

SCM
gram_output_schedule_render (SCM _output)
{
  scm_assert_smob_type (gram_output_tag, _output);

  struct gram_output *output = (struct gram_output *) SCM_SMOB_DATA (_output);
  if (output->active)
  {
    wlc_output_schedule_render (output->output);
    printf ("scheduled a render\n");
    return _output;
  }
  return SCM_ELISP_NIL;
}

void
init_gram_output_methods (void *ignore)
{
  scm_c_define_gsubr ("focus", 1, 0, 0, gram_output_focus);
  scm_c_define_gsubr ("get-name", 1, 0, 0, gram_output_get_name);
  scm_c_define_gsubr ("get-views", 1, 0, 0, gram_output_get_views);
  scm_c_define_gsubr ("get-resolution", 1, 0, 0, gram_output_get_resolution);
  scm_c_define_gsubr ("get-sleep", 1, 0, 0, gram_output_get_sleep);
  scm_c_define_gsubr ("set-views", 2, 0, 0, gram_output_set_views);
  scm_c_define_gsubr ("set-resolution", 2, 0, 0, gram_output_set_resolution);
  scm_c_define_gsubr ("set-sleep", 2, 0, 0, gram_output_set_sleep);
  scm_c_define_gsubr ("schedule-render", 1, 0, 0,
                      gram_output_schedule_render);
  scm_c_export ("focus", "schedule-render",
                /* "get-mask", */ "get-views",
                /* "get-mutable-views", */
                "get-name", "get-resolution", "get-sleep",
                /* "set-mask", */ "set-resolution", "set-sleep", "set-views",
                NULL);
}

void
init_gram_output (void)
{
  for (uint32_t i = 0; i < GRAM_MAX_OUTPUTS; i++)
  {
    output_table[i] = NULL;
    smob_table[i] = NULL;
  }

  gram_output_tag =
    scm_make_smob_type ("output", sizeof (struct gram_output));
  scm_set_smob_print (gram_output_tag, gram_output_print);
  scm_set_smob_free (gram_output_tag, gram_output_free);

  scm_c_define_module ("gram output", init_gram_output_methods, NULL);
}
