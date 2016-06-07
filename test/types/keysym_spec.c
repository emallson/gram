#include<stdlib.h>

#include <libguile.h>
#include <wlc/wlc.h>
#include <xkbcommon/xkbcommon.h>

#include <check.h>

#include "../../src/types/keysym.h"

START_TEST (test_keysym_init)
{
  scm_init_guile ();
  /* this test depends on internals of guile. do not recommend */
  init_gram_keysym ();
  scm_smob_descriptor ssd = scm_smobs[scm_numsmob - 1];

  ck_assert_str_eq (ssd.name, "keysym");
}

END_TEST
START_TEST (test_keysym_to_scm)
{
  scm_init_guile ();
  init_gram_keysym ();

  struct gram_keysym ks = {
    .keycode = XKB_KEY_x,
    .sym = XKB_KEY_x,
    .mods = {
             .leds = 0,
             .mods = WLC_BIT_MOD_ALT,
             }
  };

  SCM ks_scm = gram_keysym_scm (&ks);

  scm_assert_smob_type (gram_keysym_tag, ks_scm);

}

END_TEST
START_TEST (test_keysym_from_scm)
{
  scm_init_guile ();
  init_gram_keysym ();

  struct gram_keysym ks = {
    .keycode = XKB_KEY_x,
    .sym = XKB_KEY_x,
    .mods = {
             .leds = 0,
             .mods = WLC_BIT_MOD_ALT,
             }
  };

  SCM ks_scm = gram_keysym_scm (&ks);
  struct gram_keysym ks2 = *(struct gram_keysym *) SCM_SMOB_DATA (ks_scm);

  ck_assert_uint_eq (ks.keycode, ks2.keycode);
  ck_assert_uint_eq (ks.sym, ks2.sym);
  ck_assert_uint_eq (ks.mods.mods, ks2.mods.mods);
  ck_assert_uint_eq (ks.mods.leds, ks2.mods.leds);
}

END_TEST
START_TEST (test_keysym_kbd)
{
  scm_init_guile ();
  init_gram_keysym ();

  scm_c_use_module ("gram keysym");

  SCM res = scm_call_1 (scm_variable_ref (scm_c_lookup ("kbd")),
                        scm_from_locale_string ("M-x"));

  scm_assert_smob_type (gram_keysym_tag, res);
  struct gram_keysym ks = *(struct gram_keysym *) SCM_SMOB_DATA (res);

  ck_assert_uint_eq (ks.keycode, 0);
  ck_assert_uint_eq (ks.sym, XKB_KEY_x);
  ck_assert_uint_eq (ks.mods.mods, WLC_BIT_MOD_ALT);
  ck_assert_uint_eq (ks.mods.leds, 0);
}

END_TEST

START_TEST (test_keysym_kbd_mouse)
{
  scm_init_guile();
  init_gram_keysym();
  scm_c_use_module("gram keysym");

  SCM res = scm_call_1 (scm_variable_ref (scm_c_lookup ("kbd")),
                        scm_from_locale_string ("M-Mouse2"));

  scm_assert_smob_type (gram_keysym_tag, res);
  struct gram_keysym ks = *(struct gram_keysym *) SCM_SMOB_DATA (res);

  ck_assert_uint_eq (ks.keycode, 0);
  ck_assert_uint_eq (ks.sym, XKB_KEY_NoSymbol);
  ck_assert_uint_eq (ks.mods.mods, WLC_BIT_MOD_ALT);
  ck_assert_uint_eq (ks.mods.leds, 0);
  ck_assert_uint_eq (ks.mouse, true);
  ck_assert_uint_eq (ks.mouse_button, 2);
}
END_TEST
START_TEST (test_keysym_equalp_reflexive)
{
  scm_init_guile ();
  init_gram_keysym ();

  struct gram_keysym M_x = {
    .keycode = XKB_KEY_x,
    .sym = XKB_KEY_x,
    .mods = {
             .leds = 0,
             .mods = WLC_BIT_MOD_ALT,
             }
  };

  SCM M_x_scm = gram_keysym_scm (&M_x);
  SCM M_x_scm2 = gram_keysym_scm (&M_x);

  ck_assert (SCM_BOOL_T == scm_equal_p (M_x_scm, M_x_scm2));

  struct gram_keysym M_Mouse3 = {
    .keycode = 0,
    .sym = XKB_KEY_NoSymbol,
    .mouse = true,
    .mouse_button = 3,
    .mods = {
      .leds = 0,
      .mods = WLC_BIT_MOD_ALT,
    }
  };

  SCM M_Mouse3_scm = gram_keysym_scm (&M_Mouse3);
  SCM M_Mouse3_scm2 = gram_keysym_scm (&M_Mouse3);

  ck_assert (SCM_BOOL_T == scm_equal_p (M_Mouse3_scm, M_Mouse3_scm2));
}

END_TEST
START_TEST (test_keysym_equalp_diff_sym)
{
  scm_init_guile ();
  init_gram_keysym ();

  struct gram_keysym M_x = {
    .keycode = XKB_KEY_x,
    .sym = XKB_KEY_x,
    .mods = {
             .leds = 0,
             .mods = WLC_BIT_MOD_ALT,
             }
  };

  SCM M_x_scm = gram_keysym_scm (&M_x);
  M_x.sym = XKB_KEY_y;
  SCM M_x_scm2 = gram_keysym_scm (&M_x);

  ck_assert (SCM_BOOL_F == scm_equal_p (M_x_scm, M_x_scm2));
}

END_TEST
START_TEST (test_keysym_equalp_diff_code)
{
  scm_init_guile ();
  init_gram_keysym ();

  struct gram_keysym M_x = {
    .keycode = XKB_KEY_x,
    .sym = XKB_KEY_x,
    .mods = {
             .leds = 0,
             .mods = WLC_BIT_MOD_ALT,
             }
  };

  SCM M_x_scm = gram_keysym_scm (&M_x);
  M_x.keycode = XKB_KEY_y;
  SCM M_x_scm2 = gram_keysym_scm (&M_x);

  /* keycode is a book-keeping field that is ignored */
  ck_assert (SCM_BOOL_T == scm_equal_p (M_x_scm, M_x_scm2));
}

END_TEST
START_TEST (test_keysym_equalp_diff_mods)
{
  scm_init_guile ();
  init_gram_keysym ();

  struct gram_keysym M_x = {
    .keycode = XKB_KEY_x,
    .sym = XKB_KEY_x,
    .mods = {
             .leds = 0,
             .mods = WLC_BIT_MOD_ALT,
             }
  };

  SCM M_x_scm = gram_keysym_scm (&M_x);
  M_x.mods.mods |= WLC_BIT_MOD_CTRL;
  SCM M_x_scm2 = gram_keysym_scm (&M_x);

  ck_assert (SCM_BOOL_F == scm_equal_p (M_x_scm, M_x_scm2));
}

END_TEST
START_TEST (test_keysym_equalp_diff_leds)
{
  scm_init_guile ();
  init_gram_keysym ();

  struct gram_keysym M_x = {
    .keycode = XKB_KEY_x,
    .sym = XKB_KEY_x,
    .mods = {
             .leds = 0,
             .mods = WLC_BIT_MOD_ALT,
             }
  };

  SCM M_x_scm = gram_keysym_scm (&M_x);
  M_x.mods.leds = 1;
  SCM M_x_scm2 = gram_keysym_scm (&M_x);

  /* LEDs are currently ignored as they have no representation in
     Guile */
  ck_assert (SCM_BOOL_T == scm_equal_p (M_x_scm, M_x_scm2));
}

END_TEST
START_TEST (test_keysym_equalp_mouse)
{
  scm_init_guile ();
  init_gram_keysym ();

  struct gram_keysym M_x = {
    .keycode = XKB_KEY_x,
    .sym = XKB_KEY_x,
    .mouse = false,
    .mods = {
             .leds = 0,
             .mods = WLC_BIT_MOD_ALT,
             }
  };

  struct gram_keysym M_Mouse3 = {
    .keycode = 0,
    .sym = XKB_KEY_NoSymbol,
    .mouse = true,
    .mouse_button = 3,
    .mods = {
      .leds = 0,
      .mods = WLC_BIT_MOD_ALT,
    }
  };

  SCM M_x_scm = gram_keysym_scm (&M_x);
  SCM M_Mouse3_scm = gram_keysym_scm (&M_Mouse3);

  ck_assert (SCM_BOOL_F == scm_equal_p (M_x_scm, M_Mouse3_scm));
}

END_TEST
START_TEST (test_keysym_display)
{
  scm_init_guile ();
  scm_setlocale (scm_variable_ref (scm_c_lookup ("LC_ALL")),
                 scm_from_locale_string (""));
  init_gram_keysym ();

  struct gram_keysym M_x = {
    .keycode = XKB_KEY_x,
    .sym = XKB_KEY_x,
    .mouse = false,
    .mods = {
             .leds = 0,
             .mods = WLC_BIT_MOD_ALT,
             }
  };

  struct gram_keysym M_Mouse3 = {
    .keycode = XKB_KEY_NoSymbol,
    .sym = XKB_KEY_NoSymbol,
    .mods = {
             .leds = 0,
             .mods = WLC_BIT_MOD_ALT,
    },
    .mouse = true,
    .mouse_button = 3
  };

  SCM M_x_scm = gram_keysym_scm (&M_x);
  SCM M_Mouse3_scm = gram_keysym_scm(&M_Mouse3);
  SCM port = scm_open_output_string ();
  scm_display (M_x_scm, port);

  ck_assert_str_eq (scm_to_locale_string (scm_get_output_string (port)),
                    "#<keysym M-x>");
  scm_close (port);

  M_x.mods.mods |= WLC_BIT_MOD_LOGO | WLC_BIT_MOD_CTRL;
  M_x_scm = gram_keysym_scm (&M_x);
  port = scm_open_output_string ();

  scm_display (M_x_scm, port);
  ck_assert_str_eq (scm_to_locale_string (scm_get_output_string (port)),
                    "#<keysym S-C-M-x>");
  scm_close (port);

  M_x.sym = XKB_KEY_BackSpace;
  M_x.mods.mods = 0;
  M_x_scm = gram_keysym_scm (&M_x);
  port = scm_open_output_string ();

  scm_display (M_x_scm, port);
  ck_assert_str_eq (scm_to_locale_string (scm_get_output_string (port)),
                    "#<keysym BackSpace>");
  scm_close (port);

  port = scm_open_output_string ();
  scm_display(M_Mouse3_scm, port);
  ck_assert_str_eq(scm_to_locale_string (scm_get_output_string (port)),
                   "#<keysym M-Mouse3>");
  scm_close (port);
}

END_TEST
START_TEST (test_keysym_display_unicode)
{
  scm_init_guile ();
  scm_setlocale (scm_variable_ref (scm_c_lookup ("LC_ALL")),
                 scm_from_locale_string (""));
  init_gram_keysym ();

  struct gram_keysym M_x = {
    .keycode = XKB_KEY_x,
    .sym = XKB_KEY_udiaeresis,
    .mods = {
             .leds = 0,
             .mods = 0,
             }
  };

  SCM M_x_scm = gram_keysym_scm (&M_x);
  SCM port = scm_open_output_string ();

  scm_display (M_x_scm, port);
  ck_assert_str_eq (scm_to_utf8_string (scm_get_output_string (port)),
                    "#<keysym ü>");
  scm_close (port);

  M_x.sym = XKB_KEY_emacron;
  M_x_scm = gram_keysym_scm (&M_x);
  port = scm_open_output_string ();

  scm_display (M_x_scm, port);
  ck_assert_str_eq (scm_to_utf8_string (scm_get_output_string (port)),
                    "#<keysym ē>");
  scm_close (port);

  /* notify when we have pile of poo support */
  ck_assert_uint_eq (0,
                     xkb_keysym_from_name ("💩",
                                           XKB_KEYSYM_CASE_INSENSITIVE));
}

END_TEST
START_TEST (test_keysym_swallow)
{
  scm_init_guile ();
  init_gram_keysym ();

  ck_assert (!gram_swallow);

  scm_c_use_module ("gram keysym");

  SCM res = scm_call_0 (scm_variable_ref (scm_c_lookup ("swallow-next-key")));

  ck_assert (gram_swallow);
  ck_assert_ptr_eq (res, SCM_BOOL_T);
}

END_TEST

START_TEST (test_keysym_unmodified)
{
  scm_init_guile ();
  init_gram_keysym ();

  scm_c_use_module ("gram keysym");

  struct gram_keysym ks = {
    .keycode = XKB_KEY_x,
    .sym = XKB_KEY_x,
    .mods = {
             .leds = 0,
             .mods = WLC_BIT_MOD_ALT,
    },
    .mouse = false,
    .mouse_button = -1
  };

  SCM res = scm_call_1 (scm_variable_ref (scm_c_lookup ("unmodified")),
                        gram_keysym_scm(&ks));

  scm_assert_smob_type(gram_keysym_tag, res);
  struct gram_keysym *unmod = (struct gram_keysym*) SCM_SMOB_DATA(res);
  /* it should reset mods */
  ck_assert_uint_eq(unmod->mods.mods, 0);
  /* it shouldn't change anything else */
  ck_assert_uint_eq(unmod->mods.leds, ks.mods.leds);
  ck_assert_uint_eq(unmod->sym, ks.sym);
  ck_assert_uint_eq(unmod->keycode, ks.keycode);
  ck_assert_uint_eq(unmod->mouse, ks.mouse);
  ck_assert_uint_eq(unmod->mouse_button, ks.mouse_button);
}
END_TEST

Suite *

keysym_suite (void)
{
  Suite *s;
  TCase *tc_core, *tc_convert, *tc_equalp, *tc_display, *tc_swallow, *tc_unmod;

  s = suite_create ("types/keysym");

  tc_core = tcase_create ("Init");
  tcase_add_test (tc_core, test_keysym_init);
  suite_add_tcase (s, tc_core);

  tc_convert = tcase_create ("Convert");
  tcase_add_test (tc_convert, test_keysym_to_scm);
  tcase_add_test (tc_convert, test_keysym_from_scm);
  tcase_add_test (tc_convert, test_keysym_kbd);
  tcase_add_test (tc_convert, test_keysym_kbd_mouse);
  suite_add_tcase (s, tc_convert);

  /* testing permutations of these is left as an exercise for the
     reader */
  tc_equalp = tcase_create ("equalp");
  tcase_add_test (tc_equalp, test_keysym_equalp_reflexive);
  tcase_add_test (tc_equalp, test_keysym_equalp_diff_sym);
  tcase_add_test (tc_equalp, test_keysym_equalp_diff_code);
  tcase_add_test (tc_equalp, test_keysym_equalp_diff_mods);
  tcase_add_test (tc_equalp, test_keysym_equalp_diff_leds);
  tcase_add_test (tc_equalp, test_keysym_equalp_mouse);
  suite_add_tcase (s, tc_equalp);

  tc_display = tcase_create ("display");
  tcase_add_test (tc_display, test_keysym_display);
  tcase_add_test (tc_display, test_keysym_display_unicode);
  suite_add_tcase (s, tc_display);

  tc_swallow = tcase_create ("swallow");
  tcase_add_test (tc_swallow, test_keysym_swallow);
  suite_add_tcase (s, tc_swallow);

  tc_unmod = tcase_create("unmodified");
  tcase_add_test (tc_unmod, test_keysym_unmodified);
  suite_add_tcase (s, tc_unmod);

  return s;
}

int
main (void)
{
  int num_fail;
  Suite *s;
  SRunner *sr;

  s = keysym_suite ();
  sr = srunner_create (s);

  srunner_set_tap (sr, "-");
  srunner_run_all (sr, CK_NORMAL);
  num_fail = srunner_ntests_failed (sr);
  srunner_free (sr);
  return (num_fail == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
