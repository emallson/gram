#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

struct gram_keysym
{
  struct wlc_modifiers mods;
  uint32_t sym;
  uint32_t keycode;
};

static scm_t_bits gram_keysym_tag;

extern bool gram_swallow;

static SCM gram_keysym_equalp(SCM a, SCM b);
static int gram_keysym_print(SCM keysym_smob, SCM port, scm_print_state* pstate);
SCM gram_keysym_scm(struct gram_keysym *_keysym);
SCM gram_keysym_construct(SCM key_desc);
SCM gram_key_swallow_next(void);

void init_gram_keysym(void);
