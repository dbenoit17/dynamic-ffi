#ifndef DYNAMIC_FFI_H
#define DYNAMIC_FFI_H

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/mman.h>
#include <semaphore.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
  INTEGER,
  FLOATING,
  STRUCT,
  UNION,
  POINTER,
  ARRAY,
  VOID,
  FUNCTION,
  UNKNOWN,

  ENUM,
  TYPEDEF,
} c_type_id;


typedef enum {
  FUNCTION_DECL,
  GLOBAL_VAR_DECL,
  RECORD_DECL,
  ENUM_DECL,
  TYPEDEF_DECL
} c_decl_id;

typedef struct c_type {
  c_type_id base;
  uint64_t width;
  int is_const;
  int is_volatile;
  int is_restrict;
  int is_signed;
  int is_literal;
  int has_fields;
  unsigned int field_length;
  struct c_type *fields;
} c_type;

typedef struct {
  char *name;
  c_decl_id base;
  c_type ctype;
  char *type_str;
  void *val;
} c_decl;

typedef struct {
  uint64_t length;
  c_decl * data;
} c_decl_array;

inline c_type *c_type_pointer_deref(c_type *t) {
  return t->fields;
}

c_decl make_global_var_decl(char* name, c_type ctype, char* type_str, void *val);
c_decl make_record_decl(char* name, c_type ctype, char* type_str, void *val);
c_decl make_function_decl(char* name, c_type ctype, char* type_str, void *val);
c_decl make_enum_decl(char* name, c_type ctype, char* type_str, void *val);

c_type make_signed_int_c_type(uint64_t width, int is_const, int is_volatile);
c_type make_unsigned_int_c_type(uint64_t width, int is_const, int is_volatile);
c_type make_floating_c_type(uint64_t width, int is_const, int is_volatile);
c_type make_unknown_c_type(uint64_t width, int is_const, int is_volatile);
c_type make_array_c_type(c_type *type, int is_const, int is_volatile,
                         int is_restrict, uint64_t width);
c_type make_pointer_c_type(c_type *type, int is_const, int is_volatile,
                           int is_restrict, uint64_t width);


c_type make_struct_type(c_type* fields, int field_length, int is_const,
                        int is_volatile, uint64_t width);
c_type make_union_type(c_type* fields, int field_length, int is_const,
                        int is_volatile, uint64_t width);
c_type make_function_type(c_type* fields, int field_length);
c_type make_void_c_type(uint64_t width, int is_const, int is_volatile);

void free_decl_array(c_decl_array a);
void c_type_free_field(c_type *t);

void string_append(char **dest, const char *src,
                   unsigned int *length, unsigned int *size);

char* format_decl(c_decl d);
void print_decl(c_decl d);

typedef struct dhgc_block {
  uint64_t * begin;
  size_t size;
  size_t capacity;
  sem_t * semaphore;
} dhgc_block;

void dhgc_destroy(dhgc_block * block) {
  munmap(block->begin, block->capacity);
  munmap(block->semaphore, sizeof(sem_t));
}

void * dhgc_alloc(dhgc_block * block, size_t size) {
  uint64_t* tmp;
  sem_wait(block->semaphore);
  tmp = block->begin + (block->size / sizeof(uint64_t));
  if (block->capacity <= block->size + size) {
    printf("Exceeded dynamic-ffi shared memory limit: %d\n"
           "Try increasing the memory limit with \n"
           "   (define-dynamic-ffi #:mem-limit <mem> headers ...)\n",
           block->capacity);
    exit(1);
  }
  block->size += size;
  sem_post(block->semaphore);
  return (void*)tmp;
}

void dhgc_init(dhgc_block *block, size_t size) {
  block->begin = (uint64_t*)
    mmap(NULL, size, PROT_READ | PROT_WRITE,
         MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    block->size = 0;
    block->capacity = size;
  block->semaphore = (sem_t*)
    mmap(NULL, sizeof(sem_t), PROT_READ | PROT_WRITE,
         MAP_SHARED | MAP_ANONYMOUS, -1, 0);
  sem_init(block->semaphore, 0, 1);
}

/* These functions should not be available to the plugin*/
#ifndef DYNAMIC_FFI_PLUGIN_H
// Parse only the headers specified in argv[1:]
c_decl_array ffi_parse(size_t gc_mem_limit, dhgc_block *block,
                       int argc, const char **argv);

// Parse nested includes too
c_decl_array ffi_deep_parse(size_t gc_mem_limit, dhgc_block * block,
                            int argc, const char **argv);
#endif /* DYNAMIC_FFI_PLUGIN_H */

#ifdef __cplusplus
}
#endif

#endif /* DYNAMIC_FFI_H */
