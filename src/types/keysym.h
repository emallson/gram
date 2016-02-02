#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

struct gram_keysym
{
  struct wlc_modifiers mods;
  uint32_t sym;
  uint32_t keycode;
};


extern bool gram_swallow;
scm_t_bits gram_keysym_tag;

SCM gram_keysym_scm (struct gram_keysym *_keysym);
SCM gram_keysym_construct (SCM key_desc);
SCM gram_key_swallow_next (void);

void init_gram_keysym (void);
