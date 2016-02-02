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

scm_t_bits gram_view_tag;

void gram_view_deactivate (const wlc_handle view);
SCM gram_view_scm (const wlc_handle view);

void init_gram_view (void);
