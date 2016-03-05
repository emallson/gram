AM_CFLAGS=@GUILE_CFLAGS@ @WLC_CFLAGS@ -Wall -g
AM_LDFLAGS=@GUILE_LIBS@ @WLC_LIBS@
bin_PROGRAMS = gram
gram_SOURCES = src/gram.c src/types/keysym.c src/types/view.c		\
	src/types/output.c src/types/types.c src/hooks/hooks.c			\
	src/hooks/keydown.c src/hooks/view_created.c					\
	src/hooks/compositor_ready.c src/hooks/compositor_terminate.c	\
	src/hooks/output_created.c src/hooks/output_destroyed.c			\
	src/hooks/output_focus.c src/hooks/output_render_post.c			\
	src/hooks/output_render_pre.c src/hooks/output_resolution.c		\
	src/hooks/pointer_motion.c src/hooks/view_destroyed.c			\
	src/hooks/view_focus.c src/hooks/view_move_to_output.c			\
	src/hooks/view_render_post.c src/hooks/view_render_pre.c
gram_SOURCES_H = src/types/keysym.h src/types/view.h				\
	src/types/output.h src/types/types.h src/hooks/hooks.h			\
	src/hooks/view_created.h src/hooks/keydown.h					\
	src/hooks/compositor_ready.h src/hooks/compositor_terminate.h	\
	src/hooks/output_created.h src/hooks/output_destroyed.h			\
	src/hooks/output_focus.h src/hooks/output_render_post.h			\
	src/hooks/output_render_pre.h src/hooks/output_resolution.h		\
	src/hooks/pointer_motion.h src/hooks/view_destroyed.h			\
	src/hooks/view_focus.h src/hooks/view_move_to_output.h			\
	src/hooks/view_render_post.h src/hooks/view_render_pre.h

TESTS = keysym_check view_check
check_PROGRAMS = $(TESTS)
keysym_check_SOURCES = src/types/keysym.c test/types/keysym_spec.c
keysym_check_CFLAGS=$(AM_CFLAGS) @CHECK_CFLAGS@
keysym_check_LDFLAGS=$(AM_LDFLAGS) @CHECK_LIBS@

view_check_SOURCES = src/types/view.c src/types/output.c test/types/view_spec.c
view_check_CFLAGS=$(AM_CFLAGS) @CHECK_CFLAGS@
view_check_LDFLAGS=$(AM_LDFLAGS) @CHECK_LIBS@

TESTS_SRCS = $(keysym_check_SOURCES) $(view_check_SOURCES)

LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) \
              $(top_srcdir)/tap-driver.sh

indent: $(gram_SOURCES) $(gram_SOURCES_H) $(TESTS_SRCS)
	indent -bli0 --no-tabs $^

test: check
