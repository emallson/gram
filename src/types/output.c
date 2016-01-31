#include <libguile.h>
#include <wlc/wlc.h>

#include "output.h"

static int
gram_output_print (SCM output_smob, SCM port, scm_print_state * pstate)
{
  struct gram_output *output =
    (struct gram_output *) SCM_SMOB_DATA (output_smob);

  scm_puts ("#<output ", port);
  scm_puts (wlc_output_get_name (output->output), port);
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
          scm_gc_malloc (sizeof (struct gram_output), "output");

        *(wlc_handle *) & output_table[i]->output = output;
        break;
      }
    }
  }

  if (i >= GRAM_MAX_OUTPUTS)
  {
    /* still no room */
    return SCM_BOOL_F;
  }
  output_table[i]->active = true;
  return scm_new_smob (gram_output_tag, (scm_t_bits) output_table[i]);
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
    }
  }

  scm_gc_free (output, sizeof (struct gram_output), "output");
  return 0;
}

void
init_gram_output (void)
{
  for (uint32_t i = 0; i < GRAM_MAX_OUTPUTS; i++)
  {
    output_table[i] = NULL;
  }

  gram_output_tag =
    scm_make_smob_type ("output", sizeof (struct gram_output));
  scm_set_smob_print (gram_output_tag, gram_output_print);
  scm_set_smob_free (gram_output_tag, gram_output_free);
}
