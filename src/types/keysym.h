#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

struct gram_keysym
{
  const struct wlc_modifiers mods;
  const uint32_t sym;
  const uint32_t keycode;
};

static scm_t_bits gram_keysym_tag;

static SCM gram_keysym_equalp(SCM a, SCM b);
static int gram_keysym_print(SCM keysym_smob, SCM port, scm_print_state* pstate);
SCM gram_keysym_scm(struct gram_keysym *_keysym);

void init_gram_keysym(void);
