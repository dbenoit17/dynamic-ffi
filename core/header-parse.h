#ifndef DYNAMIC_FFI_H
#define DYNAMIC_FFI_H

#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
  ATOMIC,
  COMPOSITE
} c_type_size;

typedef enum {
  CHAR,
  UCHAR,
  INT,
  UINT,
  FLOAT,
  UFLOAT,
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

typedef struct {
  int is_const;
  int is_volatile;
  int is_restrict;
} qualifiers;

typedef struct c_type {
  c_type_size type_size;
  unsigned int width;
  qualifiers quals;
  union {
    struct {
      c_type_id id;
    } simple;
    struct {
      c_type_id id;
      unsigned int field_length;
      struct c_type *fields;
    } compound;
  } data;
} c_type;

typedef struct {
  char *name;
  c_decl_id decl_type;
  c_type type_info;
  char *qual_type;
} c_decl;

typedef struct {
  unsigned long length;
  c_decl * data;
} c_decl_array;

c_decl make_pointer_decl(char *name, char *qual_type, c_type type);

c_decl make_global_var_decl(char *name, c_type_id tid, unsigned int width,
                            qualifiers quals, char *qual_type);

c_type make_simple_c_type(c_type_id tid, unsigned int width, qualifiers quals);
c_type make_pointer_type(c_type type);
c_type make_compound_c_type(c_type_id tid, unsigned int width, qualifiers quals,
                            unsigned int field_length, c_type *fields);

inline c_type_id c_type_get_id(c_type *t) {
  return t->data.compound.id;
}

inline c_type_size c_type_get_size(c_type *t) {
  return t->type_size;
}

inline unsigned int c_type_get_field_length(c_type *t) {
  return t->data.compound.field_length;
}

#define c_type_pointer_deref c_type_get_fields
inline c_type *c_type_get_fields(c_type *t) {
  return t->data.compound.fields;
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
