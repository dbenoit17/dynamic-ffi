#include "clang-plugin.hh"
#include "clang-export.h"

/* The exporting functions need to have C linkage
   to be bound more easily to high level languages */
#ifdef __cplusplus
extern "C" {
#endif

// Parse only the headers specified in argv[1:]
c_decl_array ffi_parse(int argc, const char **argv) {
  return dynamic_ffi_parse(argc, argv, false);
}

// Parse nested includes too
c_decl_array ffi_deep_parse(int argc, const char **argv) {
  return dynamic_ffi_parse(argc, argv, true);
}

void c_type_free_fields(c_type *t) {
  if (t->has_fields) {
    c_type *fields = t->fields;
    unsigned int length = t->field_length;;
    unsigned int i;
    for (i = 0; i < length; ++i) {
      c_type_free_fields(&(fields[i]));
    }
    free(fields);
  }
}

void free_decl(c_decl d) {
  c_type * t = &(d.ctype);
  free(d.name);
  free(d.type_str);
  if (d.val != NULL) {
    free(d.val);
  }
  c_type_free_fields(t);
}

void free_decl_array(c_decl_array a) {
  unsigned int i;
  for (i = 0; i < a.length; i ++) {
    free_decl(a.data[i]);
  }
  free(a.data);
}

void string_append(char **dest, const char *src,
                   unsigned int *length, unsigned int *size) {
  unsigned int srclen = strlen(src);
  if (*size <= (*length * sizeof(char) +srclen)) {
    char *temp = (char*) malloc(sizeof(char) * (*size) *2);
    *size = *size *2 - 1;
    strcpy(temp, *dest);
    free(*dest);
    *dest = temp;
  }
  strcpy(&((*dest)[*length]), src);
  *length += srclen;
  (*dest)[*length] = '\0';
}

c_decl make_global_var_decl(char* name, c_type ctype, char* type_str, void *val) {
  c_decl d;
  d.name = name;
  d.base = GLOBAL_VAR_DECL;
  d.ctype = ctype;
  d.type_str = type_str;
  d.val = val;

  return d;
}

c_decl make_record_decl(char* name, c_type ctype, char* type_str, void * val) {
  c_decl d;
  d.name = name;
  d.base = RECORD_DECL;
  d.ctype = ctype;
  d.type_str = type_str;
  d.val = val;

  return d;
}

c_decl make_typedef_decl(char* name, c_type ctype, char* type_str, void * val) {
  c_decl d;
  d.name = name;
  d.base = TYPEDEF_DECL;
  d.ctype = ctype;
  d.type_str = type_str;
  d.val = val;

  return d;
}

c_decl make_function_decl(char* name, c_type ctype, char* type_str, void *val) {
  c_decl d;
  d.name = name;
  d.base = FUNCTION_DECL;
  d.ctype = ctype;
  d.type_str = type_str;
  d.val = val;

  return d;
}

c_decl make_enum_decl(char* name, c_type ctype, char* type_str, void *val) {
  c_decl d;
  d.name = name;
  d.base = ENUM_DECL;
  d.ctype = ctype;
  d.type_str = type_str;
  d.val = val;

  return d;
}

c_type make_signed_int_c_type(uint64_t width, int is_const, int is_volatile) {
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

c_type make_unsigned_int_c_type(uint64_t width, int is_const, int is_volatile) {
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

c_type make_floating_c_type(uint64_t width, int is_const, int is_volatile) {
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

c_type make_unknown_c_type(uint64_t width, int is_const, int is_volatile) {
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

c_type make_void_c_type(uint64_t width, int is_const, int is_volatile) {
    c_type t;
    t.base = VOID;
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

c_type make_pointer_c_type(c_type type, int is_const, int is_volatile, int is_restrict, uint64_t width) {
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
c_type make_array_c_type(c_type type, int is_const, int is_volatile, int is_restrict, uint64_t width) {
  c_type t;
  t.base = ARRAY;
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

c_type make_struct_type(c_type* fields, int field_length, int is_const, int is_volatile, uint64_t width) {
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

c_type make_union_type(c_type* fields, int field_length, int is_const, int is_volatile, uint64_t width) {
  c_type t;
  t.base = UNION;
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

c_type make_function_type(c_type* fields, int field_length) {
  c_type t;
  t.base = FUNCTION;
  t.fields = fields;
  t.has_fields = 1;
  t.field_length = field_length;
  t.is_signed = 0;
  t.is_literal = 0;
  t.is_const = 0;
  t.is_volatile = 0;
  t.is_restrict = 0;
  t.width = 0;
  return t;
}

#ifdef __cplusplus
} /* end extern C */
#endif
