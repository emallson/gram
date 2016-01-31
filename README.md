Gram
====

Gram is a highly experimental Wayland window manager.

Eventually, it will be a tiling window manager, but for now it just
throws things on the screen.

Dependencies
============

[Cloudef/wlc](https://github.com/Cloudef/wlc)
[libguile](http://www.gnu.org/software/guile/)

If you're on linux, you almost certainly already have guile installed
as it is an optional dependency of many GNU programs including `gdb`.

Compiling
=========

    automake --add-missing
    autoreconf
    ./configure
    make
