#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

/* I should probably just unbox this, not entirely sure if I'll need
   to track more state with it though */
struct gram_view
{
  const wlc_handle view;
};

static scm_t_bits gram_view_tag;

static SCM gram_view_equalp(SCM a, SCM b);
static int gram_view_print(SCM view_smob, SCM port, scm_print_state* pstate);
SCM gram_view_scm(struct gram_view *_view);

void init_gram_view(void);

/* function exposures */
static SCM gram_view_close_tag;

static SCM gram_view_bring_to_front_tag;
static SCM gram_view_send_to_back_tag;
static SCM gram_view_focus_tag;

static SCM gram_view_get_geometry_tag;
static SCM gram_view_get_state_tag;
static SCM gram_view_get_mask_tag;
static SCM gram_view_get_output_tag;
static SCM gram_view_get_class_tag;
static SCM gram_view_get_type_tag;
static SCM gram_view_get_app_id_tag;

static SCM gram_view_set_geometry_tag;
static SCM gram_view_set_mask_tag;
static SCM gram_view_set_state_tag;
