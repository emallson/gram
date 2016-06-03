#include "keydown.h"
#include "keyup.h"

#include "view_created.h"
#include "view_destroyed.h"
#include "view_focus.h"
#include "view_move_to_output.h"
#include "view_render_pre.h"
#include "view_render_post.h"
#include "view_request_geometry.h"

#include "output_created.h"
#include "output_destroyed.h"
#include "output_focus.h"
#include "output_render_pre.h"
#include "output_render_post.h"
#include "output_resolution.h"

#include "pointer_motion.h"

#include "compositor_ready.h"
#include "compositor_terminate.h"

void init_gram_hooks (void);
