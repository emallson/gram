#include "hooks.h"

void
init_gram_hooks (void)
{
  gram_keydown_hook_init ();
  gram_view_created_hook_init ();
}
