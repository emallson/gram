bin_PROGRAMS = gram
gram_SOURCES = src/gram.c src/types/keysym.c src/types/view.c src/types/output.c
gram_SOURCES_H = src/types/keysym.h src/types/view.h src/types/output.h
gram_CFLAGS=@GUILE_CFLAGS@ @WLC_CFLAGS@
gram_LDFLAGS=@GUILE_LIBS@ @WLC_LIBS@

indent: $(gram_SOURCES) $(gram_SOURCES_H)
	indent -bli0 --no-tabs $^
