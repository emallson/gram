#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

struct gram_view
{
  const wlc_handle view;
  bool active;
};

/* I look forward to the day that someone yells at me for this */
#define GRAM_MAX_VIEWS 4096
static struct gram_view *view_table[GRAM_MAX_VIEWS];

static scm_t_bits gram_view_tag;

/* static SCM gram_view_equalp (SCM a, SCM b); */
static int gram_view_print (SCM view_smob, SCM port,
                            scm_print_state * pstate);
static size_t gram_view_free (SCM _view);
void gram_view_deactivate (const wlc_handle view);
SCM gram_view_scm (const wlc_handle view);

void init_gram_view (void);
