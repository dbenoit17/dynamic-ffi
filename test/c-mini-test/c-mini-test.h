/* Copyright (c) 2016 David Benoit
   unit_test.h

   Some unit testing utilities.

*/
#include <stdio.h>

#ifndef UNIT_TEST_H
#define UNIT_TEST_H
 
#define ERR_LENGTH_MAX 128
#define MAX_NUM_ERRORS 64


#define TEST_BEGIN(N) \
	test_result _reserved_test_result_name_; \
	test_result_init(&_reserved_test_result_name_); \
	set_test_name(&_reserved_test_result_name_, (N)); \

#define TEST_CASE(E, F) test_case(&_reserved_test_result_name_, (E), (F));

#define TEST_END() return _reserved_test_result_name_;

#define RUN_TEST_SUITE(X,Y) run_test_suite(X,Y,sizeof(X) / sizeof(unit_test))

enum blah {
  DE,
  BLAH,
  BLECH
};

struct test_result {
  char test_name[ERR_LENGTH_MAX];
  unsigned int num_errors;
  char error_list[MAX_NUM_ERRORS][ERR_LENGTH_MAX];
};

typedef struct test_result (*unit_test)(void);
typedef struct test_result test_result;

void test_result_init(test_result* t);
/* Set the name of the current test. */
void set_test_name(test_result*, char *test_name);
/* Run a test case */
void test_case(test_result*, int expression, char* fail_notification);
/* Run a test suite */
int run_test_suite(const unit_test* array_of_tests, char *test_suite_name, unsigned int num_tests);

void use_blah(enum blah);

int z;

#endif
