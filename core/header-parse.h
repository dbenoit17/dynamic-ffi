#ifndef DYNAMIC_FFI_H
#define DYNAMIC_FFI_H

#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
  INTEGER,
  FLOATING,
  ENUM,
  STRUCT,
  UNION,
  POINTER,
  FUNCTION,
  TYPEDEF,
  UNKNOWN,
} c_type_id;

typedef enum {
  FUNCTION_DECL,
  GLOBAL_VAR_DECL,
  STRUCT_DECL,
  UNION_DECL,
  TYPEDEF_DECL
} c_decl_id;

typedef struct c_type {
  c_type_id base;
  unsigned int width;
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
} c_decl;

typedef struct {
  unsigned long length;
  c_decl * data;
} c_decl_array;

inline c_type *c_type_pointer_deref(c_type *t) {
  return t->fields;
}

c_decl make_global_var_decl(char* name, c_type ctype, char* type_str) {
  c_decl d;
  d.name = name;
  d.base = GLOBAL_VAR_DECL;
  d.ctype = ctype;
  d.type_str = type_str;

  return d;
}

c_decl make_struct_decl(char* name, c_type ctype, char* type_str) {
  c_decl d;
  d.name = name;
  d.base = STRUCT_DECL;
  d.ctype = ctype;
  d.type_str = type_str;

  return d;
}

c_type make_signed_int_c_type(unsigned int width, int is_const, int is_volatile) {
    c_type t;
    t.base = INTEGER;
    t.width = width;
    t.is_const = is_const;
    t.is_volatile = is_volatile;
    t.is_restrict = 0;
    t.is_signed = 1;
    t.is_literal = 0;
    t.has_fields = 0;
    t.field_length = 0;
    t.fields = NULL;
    return t;
}

c_type make_unsigned_int_c_type(unsigned int width, int is_const, int is_volatile) {
    c_type t;
    t.base = INTEGER;
    t.width = width;
    t.is_const = is_const;
    t.is_volatile = is_volatile;
    t.is_restrict = 0;
    t.is_signed = 0;
    t.is_literal = 0;
    t.has_fields = 0;
    t.field_length = 0;
    t.fields = NULL;
    return t;
}

c_type make_floating_c_type(unsigned int width, int is_const, int is_volatile) {
    c_type t;
    t.base = FLOATING;
    t.width = width;
    t.is_const = is_const;
    t.is_volatile = is_volatile;
    t.is_restrict = 0;
    t.is_signed = 0;
    t.is_literal = 0;
    t.has_fields = 0;
    t.field_length = 0;
    t.fields = NULL;
    return t;
}

c_type make_unknown_c_type(unsigned int width, int is_const, int is_volatile) {
    c_type t;
    t.base = UNKNOWN;
    t.width = width;
    t.is_const = is_const;
    t.is_volatile = is_volatile;
    t.is_restrict = 0;
    t.is_signed = 0;
    t.is_literal = 0;
    t.has_fields = 0;
    t.field_length = 0;
    t.fields = NULL;
    return t;
}

c_type make_pointer_c_type(c_type type, int is_const, int is_volatile, int is_restrict, unsigned int width) {
  c_type t;
  t.base = POINTER;
  t.fields = (c_type*) malloc(sizeof(c_type));
  t.has_fields = 1;
  memcpy(t.fields, &type, sizeof(c_type));
  t.field_length = 1;
  t.is_signed = 0;
  t.is_literal = 0;
  t.is_const = is_const;
  t.is_volatile = is_volatile;
  t.is_restrict = is_restrict;
  t.width = width;
  return t;
}

c_type make_struct_type(c_type* fields, int field_length, int is_const, int is_volatile, unsigned int width) {
  c_type t;
  t.base = STRUCT;
  t.fields = fields;
  t.has_fields = 1;
  t.field_length = field_length;
  t.is_signed = 0;
  t.is_literal = 0;
  t.is_const = is_const;
  t.is_volatile = is_volatile;
  t.is_restrict = 0;
  t.width = width;
  return t;
}

const char* c_type_get_size_str(c_type s);
const char* decl_type_get_str(c_decl d);
const char* c_type_get_str(c_type s);

void free_decl_array(c_decl_array a);
void c_type_free_field(c_type *t);

void string_append(char **dest, const char *src,
                   unsigned int *length, unsigned int *size);
char* format_decl(c_decl d);
void print_decl(c_decl d);

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
