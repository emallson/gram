AC_PREREQ([2.61])
AC_INIT([gram], [0.1])
AC_PROG_CC([clang gcc])
AM_INIT_AUTOMAKE([-Wall -Werror foreign subdir-objects])
AM_SILENT_RULES([yes])

AC_REQUIRE_AUX_FILE([tap-driver.sh])

AC_CONFIG_SRCDIR([src/gram.c])
AC_CONFIG_HEADERS([src/config.h])

AC_CHECK_HEADERS([stdlib.h])
AC_CHECK_HEADERS([unistd.h])
AC_CHECK_HEADER_STDBOOL

AC_TYPE_UINT32_T

PKG_CHECK_MODULES([GUILE], [guile-2.0])

PKG_CHECK_MODULES([WLC], [wlc])

PKG_CHECK_MODULES([CHECK], [check])

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
