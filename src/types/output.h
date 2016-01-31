#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

struct gram_output
{
  const wlc_handle output;
  bool active;
};

/* 32 outputs should be enough for everyone, right? RIGHT? */
#define GRAM_MAX_OUTPUTS 32
static struct gram_output *output_table[GRAM_MAX_OUTPUTS];

static scm_t_bits gram_output_tag;

static int gram_output_print (SCM output_smob, SCM port,
                              scm_print_state * pstate);
static size_t gram_output_free (SCM _output);
SCM gram_output_scm (const wlc_handle output);
void gram_output_deactivate (const wlc_handle output);

void init_gram_output (void);
