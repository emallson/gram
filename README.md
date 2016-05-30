Gram
====

Gram is a highly experimental Wayland window manager.

*Current Status:* Nearly all basic functionality is present. The major missing piece is completion of the floating layer / mouse support.

*Completed Functionality*:

- Workspaces
- Nestable Layouts
- Intra-layout window & cursor motion
- Rudimentary floating layer (appropriate windows (eg `dmenu`) are floated, but there is presently no way to move/resize/focus them if you move focus away from them)

*To-Be-Done*:

- Inter-layout cursor motion (issue [#3](../../issues/3), issue [#9](../../issues/9))
- Complete floating layer / mouse support (issues [#3](../../issues/3), [#4](../../issues/4))
- Public functions to insert and rearrange nested layouts (issue [#9](../../issues/9))

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
    make SCHEME_DIR=./lib
