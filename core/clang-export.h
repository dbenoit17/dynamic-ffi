/* This header declares C types and constructors
   for storing AST metadata extracted from clang. */

#ifndef DYNAMIC_FFI_H
#define DYNAMIC_FFI_H

#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* The exporting functions need to have C linkage
   to be bound more easily to high level languages */
#ifdef __cplusplus
extern "C" {
#endif

/* enum to represent C builtin types */
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


/* enum to categorize C declarations */
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
  c_decl_id base;
  char *name;
  char *type_str;
  size_t val_size;
  void *val;
  c_type ctype;
} c_decl;

typedef struct {
  unsigned long length;
  c_decl * data;
} c_decl_array;

inline c_type *c_type_pointer_deref(c_type *t) {
  return t->fields;
}

/* constructor fwd decls */
c_decl make_global_var_decl(char* name, c_type ctype, char* type_str, void *val);
c_decl make_typedef_decl(char* name, c_type ctype, char* type_str, void *val);
c_decl make_record_decl(char* name, c_type ctype, char* type_str, void *val);
c_decl make_function_decl(char* name, c_type ctype, char* type_str, void *val);
c_decl make_enum_decl(char* name, c_type ctype, char* type_str, void *val);

c_type make_signed_int_c_type(uint64_t width, int is_const, int is_volatile);
c_type make_unsigned_int_c_type(uint64_t width, int is_const, int is_volatile);
c_type make_floating_c_type(uint64_t width, int is_const, int is_volatile);
c_type make_unknown_c_type(uint64_t width, int is_const, int is_volatile);
c_type make_array_c_type(c_type type, int is_const, int is_volatile,
                         int is_restrict, uint64_t width);
c_type make_pointer_c_type(c_type type, int is_const, int is_volatile,
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

/* These functions should not be available to the plugin*/
#ifndef DYNAMIC_FFI_PLUGIN_H
// Parse only the headers specified in argv[1:]
c_decl_array ffi_parse(int argc, const char **argv);

// Parse nested includes too
c_decl_array ffi_deep_parse(int argc, const char **argv);
#endif /* DYNAMIC_FFI_PLUGIN_H */

#ifdef __cplusplus
}
#endif

#endif /* DYNAMIC_FFI_H */
