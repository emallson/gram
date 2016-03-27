Gram
====

Gram is a highly experimental Wayland window manager.

*Current Status:* Bare-bones. Basic column, row, and tall
[layouts](lib/gram/lib/layout.scm) implemented with lifecycle
management for views, but no:

- Window Motion (easy)
- Floating Layer (harder)
- Mouse support (hard)
- Workspaces (???)

Dependencies
============

[Cloudef/wlc](https://github.com/Cloudef/wlc)

[libguile](http://www.gnu.org/software/guile/)

If you're on linux, you almost certainly already have guile installed
as it is an optional dependency of many GNU programs including `gdb`.

Compiling
=========

    autoreconf --install
    ./configure
    make
