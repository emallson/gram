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

SCM gram_output_scm (const wlc_handle output);
void gram_output_deactivate (const wlc_handle output);

void init_gram_output (void);
