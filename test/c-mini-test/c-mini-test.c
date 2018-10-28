/* Copyright (c) 2016 David Benoit
   unit_test.c
   
   Implementation of unit test utilities
*/
#include <string.h>
#include <stdio.h>
#include "c-mini-test.h"

void test_result_init(test_result* t) {
  t->num_errors = 0;
  strncpy(t->test_name, "Unknown Test", ERR_LENGTH_MAX);
}

void set_test_name(test_result* t, char* test_name) {
  strncpy(t->test_name, test_name, ERR_LENGTH_MAX);
}

void test_case(test_result* t, int expression, char* fail_notification) {
  if (!expression) {
    if (t->num_errors < MAX_NUM_ERRORS) {
      strncpy(t->error_list[t->num_errors], fail_notification, ERR_LENGTH_MAX);
      t->num_errors++;
    }
    else {
      printf("WARNING: cannot append test failure: %s to test_result."
             "  MAX_NUM_ERRORS reached\n", 
             fail_notification);
    }
  }
}   

int execute_test(unit_test test) {
  int i;
  test_result t; 
  t = test();
  if (t.num_errors == 0) {
    printf("PASSED: %s\n", t.test_name);
    return 0;
  }
  else   {
    printf("FAILED: %s \n    WITH ERRORS: ", t.test_name);
    for (i = 0; i < t.num_errors; ++i)
      printf("%s\n                 ", t.error_list[i]);
    return t.num_errors;
  }
}

int run_test_suite(const unit_test *array_of_tests, char *test_suite_name, unsigned int num_tests)
{
  int i, passed=0, failed=0, errors=0;
  printf("Running Test Suite: %s\n", test_suite_name);
  for (i = 0; i < num_tests; ++i) {
    int current_test_errors = execute_test(array_of_tests[i]);
    if (current_test_errors > 0) {
      ++failed;
      errors += current_test_errors;
    }
    else {
      ++passed;
    }
  } 
  printf("\nTOTAL PASSED: %d\nTOTAL FAILED: %d\nTOTAL ERRORS: %d\n", passed, failed, errors); 
  return 0;
}

