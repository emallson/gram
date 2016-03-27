#pragma once
#include <libguile.h>
#include <wlc/wlc.h>

struct view_geo
{
  wlc_handle view;
  const struct wlc_geometry *geo;
};

void gram_view_request_geometry_hook_init (void);
void *gram_view_request_geometry_hook_run (void *data);
