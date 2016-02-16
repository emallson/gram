#include<stdlib.h>

#include <libguile.h>
#include <wlc/wlc.h>

#include <check.h>

#include "../../src/types/view.h"

START_TEST (test_view_init)
{
  scm_init_guile ();
  /* this test depends on internals of guile. do not recommend */
  init_gram_view ();
  scm_smob_descriptor ssd = scm_smobs[scm_numsmob - 1];

  ck_assert_str_eq (ssd.name, "view");
}

END_TEST
START_TEST (test_view_convert)
{
  scm_init_guile ();
  init_gram_view ();

  /* wlc_handle is a typedef of uint32_t. Calling any wlc_* functions
     on these WILL FAIL. However, it allows testing smob conversion in
     isolation. */
  const wlc_handle a = 1, b = 2;

  SCM sa = gram_view_scm (a), sb = gram_view_scm (b);

  scm_assert_smob_type (gram_view_tag, sa);
  scm_assert_smob_type (gram_view_tag, sb);
}

END_TEST
START_TEST (test_view_deactivate_basic)
{
  scm_init_guile ();
  init_gram_view ();

  const wlc_handle a = 1;

  SCM sa = gram_view_scm (a);

  ck_assert (((struct gram_view *) SCM_SMOB_DATA (sa))->active);

  gram_view_deactivate (a);

  ck_assert (!((struct gram_view *) SCM_SMOB_DATA (sa))->active);
}

END_TEST
START_TEST (test_view_reactivated_conversion)
{
  /* conversion should reactivate a view because it is again safe to
     use. This is predicated on gram_view_scm only being called on
     active views, and gram_view_deactivate being called immediately
     when a view is closed. */
  scm_init_guile ();
  init_gram_view ();

  const wlc_handle a = 1;

  SCM sa = gram_view_scm (a);

  ck_assert (((struct gram_view *) SCM_SMOB_DATA (sa))->active);

  gram_view_deactivate (a);
  SCM sa2 = gram_view_scm (a);

  ck_assert (((struct gram_view *) SCM_SMOB_DATA (sa2))->active);
  ck_assert (((struct gram_view *) SCM_SMOB_DATA (sa))->active);
}

END_TEST
START_TEST (test_view_deactivate_fns)
{
  /* it should be safe to call any gram_view_* fns on a deactivated
     view, regardless of WLC's state. */
  scm_init_guile ();
  init_gram_view ();

  const wlc_handle a = 1;

  SCM sa = gram_view_scm (a);

  ck_assert (((struct gram_view *) SCM_SMOB_DATA (sa))->active);

  gram_view_deactivate (a);

  scm_c_use_module ("gram view");
  /* mutations should return the view itself */
  ck_assert_ptr_eq (scm_call_1
                    (scm_variable_ref (scm_c_lookup ("close")), sa), sa);
  ck_assert_ptr_eq (scm_call_1
                    (scm_variable_ref (scm_c_lookup ("focus")), sa), sa);
  ck_assert_ptr_eq (scm_call_1
                    (scm_variable_ref (scm_c_lookup ("bring-to-front")),
                     sa), sa);
  ck_assert_ptr_eq (scm_call_1
                    (scm_variable_ref (scm_c_lookup ("send-to-back")),
                     sa), sa);
  /* getters should return #f */
  ck_assert_ptr_eq (scm_call_1
                    (scm_variable_ref (scm_c_lookup ("get-geometry")),
                     sa), SCM_BOOL_F);
  ck_assert_ptr_eq (scm_call_1
                    (scm_variable_ref (scm_c_lookup ("get-state")), sa),
                    SCM_BOOL_F);
  ck_assert_ptr_eq (scm_call_1
                    (scm_variable_ref (scm_c_lookup ("get-output")), sa),
                    SCM_BOOL_F);
  ck_assert_ptr_eq (scm_call_1
                    (scm_variable_ref (scm_c_lookup ("get-app-id")), sa),
                    SCM_BOOL_F);
  ck_assert_ptr_eq (scm_call_1
                    (scm_variable_ref (scm_c_lookup ("get-class")), sa),
                    SCM_BOOL_F);
  ck_assert_ptr_eq (scm_call_1
                    (scm_variable_ref (scm_c_lookup ("get-type")), sa),
                    SCM_BOOL_F);
}

END_TEST
START_TEST (test_view_equalp_reflexive)
{
  scm_init_guile ();
  init_gram_view ();

  const wlc_handle a = 1, b = 2;

  SCM sa = gram_view_scm (a), sb = gram_view_scm (b);

  ck_assert_ptr_eq (scm_equal_p (sa, sa), SCM_BOOL_T);
  ck_assert_ptr_eq (scm_equal_p (sb, sb), SCM_BOOL_T);
}

END_TEST
START_TEST (test_view_equalp_pseudoreflexive)
{
  /* if a == b then (equalp sa sb) */
  scm_init_guile ();
  init_gram_view ();

  const wlc_handle a = 1, b = 1;

  SCM sa = gram_view_scm (a), sb = gram_view_scm (b);

  ck_assert_ptr_eq (scm_eq_p (sa, sb), SCM_BOOL_T);
  ck_assert_ptr_eq (scm_equal_p (sa, sb), SCM_BOOL_T);
}

END_TEST
START_TEST (test_view_equalp_nontrivial)
{
  /* if a != b then (not (equalp sa sb)) */
  scm_init_guile ();
  init_gram_view ();

  const wlc_handle a = 1, b = 2;

  SCM sa = gram_view_scm (a), sb = gram_view_scm (b);

  ck_assert_ptr_eq (scm_equal_p (sa, sb), SCM_BOOL_F);
}

END_TEST Suite *
view_suite (void)
{
  Suite *s;
  TCase *tc_init, *tc_convert, *tc_deactivate, *tc_equalp;

  s = suite_create ("types/view");

  tc_init = tcase_create ("init");
  tcase_add_test (tc_init, test_view_init);
  suite_add_tcase (s, tc_init);

  tc_convert = tcase_create ("convert");
  tcase_add_test (tc_convert, test_view_convert);
  suite_add_tcase (s, tc_convert);

  tc_deactivate = tcase_create ("deactivate");
  tcase_add_test (tc_deactivate, test_view_deactivate_basic);
  tcase_add_test (tc_deactivate, test_view_reactivated_conversion);
  tcase_add_test (tc_deactivate, test_view_deactivate_fns);
  suite_add_tcase (s, tc_deactivate);

  tc_equalp = tcase_create ("equalp");
  tcase_add_test (tc_equalp, test_view_equalp_reflexive);
  tcase_add_test (tc_equalp, test_view_equalp_pseudoreflexive);
  tcase_add_test (tc_equalp, test_view_equalp_nontrivial);
  suite_add_tcase (s, tc_equalp);

  return s;
}

int
main (void)
{
  int num_fail;
  Suite *s;
  SRunner *sr;

  s = view_suite ();
  sr = srunner_create (s);

  srunner_set_tap (sr, "-");
  srunner_run_all (sr, CK_NORMAL);
  num_fail = srunner_ntests_failed (sr);
  srunner_free (sr);
  return (num_fail == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
