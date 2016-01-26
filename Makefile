package = gram
version = 0.1
tarname = gram

LIBS=-lguile-2.0 -lgc -lwlc -lxkbcommon -linput
CFLAGS=-pthread -I/usr/include/guile/2.0 -I/usr/include/pixman-1
CC=gcc

all: gram

gram: src/gram.c
	$(CC) $(LIBS) $(CFLAGS) -o $@ $+
