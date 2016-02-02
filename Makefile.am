AM_CFLAGS=@GUILE_CFLAGS@ @WLC_CFLAGS@ -Wall -g
AM_LDFLAGS=@GUILE_LIBS@ @WLC_LIBS@
bin_PROGRAMS = gram
gram_SOURCES = src/gram.c \
	src/types/keysym.c src/types/view.c src/types/output.c src/types/types.c \
	src/hooks/hooks.c src/hooks/keydown.c src/hooks/view_created.c
gram_SOURCES_H = src/types/keysym.h src/types/view.h src/types/output.h src/types/types.h \
	src/hooks/hooks.h src/hooks/view_created.h src/hooks/keydown.h

indent: $(gram_SOURCES) $(gram_SOURCES_H)
	indent -bli0 --no-tabs $^
