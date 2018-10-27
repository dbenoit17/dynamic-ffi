#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "escheme.h"
#include "header-parse.h"

#define DEFINE_STRUCT_INITIALIZER(NAME, SUPER, NUM_FIELDS, FIELDS ...) \
const char* struct_##NAME##_fields[NUM_FIELDS] = {FIELDS}; \
Scheme_Object * NAME; \
void struct_##NAME##_init(Scheme_Env * env) { \
  Scheme_Object *lst; \
  Scheme_Object *name_symbol; \
  Scheme_Object **struct_names; \
  Scheme_Object **struct_values; \
  Scheme_Object* struct_names_array[NUM_FIELDS]; \
  int count, i; \
  char * name_with_dash; \
\
  name_with_dash = dashify(#NAME); \
  name_symbol = scheme_intern_symbol(name_with_dash); \
  free(name_with_dash); \
\
  for (i = 0; i < NUM_FIELDS; ++i) { \
    struct_names_array[i] = \
      scheme_intern_symbol(struct_##NAME##_fields[i]); \
  } \
\
  NAME = scheme_make_struct_type(name_symbol, SUPER, NULL, NUM_FIELDS, 0, NULL, \
      scheme_build_list(0,NULL), NULL); \
  lst = scheme_build_list(NUM_FIELDS, struct_names_array); \
  struct_names = \
     scheme_make_struct_names(name_symbol, lst, struct_flags, &count); \
  struct_values = \
    scheme_make_struct_values(NAME, \
      struct_names, count, struct_flags); \
\
  for (i = 0; i < count; ++i) { \
    scheme_add_global( \
      scheme_symbol_val(struct_names[i]), struct_values[i], env); \
  } \
}

#define INITIALIZE_STRUCT(NAME,E) struct_##NAME##_init((E));

const int struct_flags = SCHEME_STRUCT_NO_MAKE_PREFIX; 
Scheme_Object *dynamic_ffi_parse();
char *dashify(const char *str);

DEFINE_STRUCT_INITIALIZER(declaration, NULL, 3, "name", "type", "type-string");
DEFINE_STRUCT_INITIALIZER(func_decl, declaration, 0);
DEFINE_STRUCT_INITIALIZER(var_decl, declaration, 0);
DEFINE_STRUCT_INITIALIZER(struct_decl, declaration, 0);
DEFINE_STRUCT_INITIALIZER(union_decl, declaration, 0);

DEFINE_STRUCT_INITIALIZER(ctype, NULL, 0);
DEFINE_STRUCT_INITIALIZER(ctype_atomic, ctype, 0);

DEFINE_STRUCT_INITIALIZER(ctype_int, ctype, 0);

DEFINE_STRUCT_INITIALIZER(ctype_signed_int, ctype_int, 0);
DEFINE_STRUCT_INITIALIZER(ctype_int8,  ctype_signed_int, 0);
DEFINE_STRUCT_INITIALIZER(ctype_int16, ctype_signed_int, 0);
DEFINE_STRUCT_INITIALIZER(ctype_int32, ctype_signed_int, 0);
DEFINE_STRUCT_INITIALIZER(ctype_int64, ctype_signed_int, 0);

DEFINE_STRUCT_INITIALIZER(ctype_unsigned_int, ctype_int, 0);
DEFINE_STRUCT_INITIALIZER(ctype_uint8,  ctype_unsigned_int, 0);
DEFINE_STRUCT_INITIALIZER(ctype_uint16, ctype_unsigned_int, 0);
DEFINE_STRUCT_INITIALIZER(ctype_uint32, ctype_unsigned_int, 0);
DEFINE_STRUCT_INITIALIZER(ctype_uint64, ctype_unsigned_int, 0);

DEFINE_STRUCT_INITIALIZER(ctype_float, ctype, 0);

DEFINE_STRUCT_INITIALIZER(ctype_signed_float, ctype_float, 0);
DEFINE_STRUCT_INITIALIZER(ctype_float32, ctype_signed_float, 0);
DEFINE_STRUCT_INITIALIZER(ctype_float64, ctype_signed_float, 0);

DEFINE_STRUCT_INITIALIZER(ctype_unsigned_float, ctype_float, 0);
DEFINE_STRUCT_INITIALIZER(ctype_ufloat32, ctype_unsigned_float, 0);
DEFINE_STRUCT_INITIALIZER(ctype_ufloat64, ctype_unsigned_float, 0);

DEFINE_STRUCT_INITIALIZER(ctype_composite, ctype, 0);
DEFINE_STRUCT_INITIALIZER(ctype_struct, ctype_composite, 0);
DEFINE_STRUCT_INITIALIZER(ctype_union, ctype_composite, 0);
DEFINE_STRUCT_INITIALIZER(ctype_pointer, ctype_composite, 0);
DEFINE_STRUCT_INITIALIZER(ctype_function, ctype_composite, 0);

Scheme_Object *scheme_initialize(Scheme_Env *env) {
  Scheme_Env *mod_env;
  Scheme_Object * result; 

  mod_env = scheme_primitive_module(scheme_intern_symbol("dynamic-ffi-core"), env);

  //result = dynamic_ffi_parse();
  //scheme_debug_print(scheme_make_utf8_string("parsing complete\n"));

  INITIALIZE_STRUCT(declaration, mod_env);
  INITIALIZE_STRUCT(func_decl, mod_env);
  INITIALIZE_STRUCT(var_decl, mod_env);
  INITIALIZE_STRUCT(struct_decl, mod_env);
  INITIALIZE_STRUCT(union_decl, mod_env);

  INITIALIZE_STRUCT(ctype, mod_env);
  INITIALIZE_STRUCT(ctype_atomic, mod_env);
  
  INITIALIZE_STRUCT(ctype_int, mod_env);
  
  INITIALIZE_STRUCT(ctype_signed_int, mod_env);
  INITIALIZE_STRUCT(ctype_int8,  mod_env);
  INITIALIZE_STRUCT(ctype_int16, mod_env);
  INITIALIZE_STRUCT(ctype_int32, mod_env);
  INITIALIZE_STRUCT(ctype_int64, mod_env);
  
  INITIALIZE_STRUCT(ctype_unsigned_int, mod_env);
  INITIALIZE_STRUCT(ctype_uint8,  mod_env);
  INITIALIZE_STRUCT(ctype_uint16, mod_env);
  INITIALIZE_STRUCT(ctype_uint32, mod_env);
  INITIALIZE_STRUCT(ctype_uint64, mod_env);
  
  INITIALIZE_STRUCT(ctype_float, mod_env);
  
  INITIALIZE_STRUCT(ctype_signed_float, mod_env);
  INITIALIZE_STRUCT(ctype_float32, mod_env);
  INITIALIZE_STRUCT(ctype_float64, mod_env);
  
  INITIALIZE_STRUCT(ctype_unsigned_float, mod_env);
  INITIALIZE_STRUCT(ctype_ufloat32, mod_env);
  INITIALIZE_STRUCT(ctype_ufloat64, mod_env);
  
  
  INITIALIZE_STRUCT(ctype_composite, mod_env);
  INITIALIZE_STRUCT(ctype_struct, mod_env);
  INITIALIZE_STRUCT(ctype_union, mod_env);
  INITIALIZE_STRUCT(ctype_pointer, mod_env);
  INITIALIZE_STRUCT(ctype_function, mod_env);
    
  scheme_finish_primitive_module(mod_env);
  return scheme_void;
}

Scheme_Object *scheme_reload(Scheme_Env *env) {
  return scheme_initialize(env);
}

Scheme_Object *scheme_module_name() {
  return scheme_intern_symbol("dynamic-ffi-core");
}

Scheme_Object *dynamic_ffi_parse() {
  const int argc = 2;
  const char *argv[2]; 
  int i;
  c_decl_array decls;

  argv[0] = "dummy"; 
  argv[1] = "/home/dbenoit/Documents/adqc-ffi/dynamic-ffi/test/test-prg.c";
  decls = ffi_parse(argc, argv);
  free_decl_array(decls);
  
  return scheme_make_utf8_string("done");
}

char * dashify(const char* str) {
  char * buffer;
  unsigned int i, length;

  length  = strlen(str);
  buffer = malloc(sizeof(char) * (length + 1));
  for (i = 0; i < length; ++i) {
    if (str[i] != '_') {
      buffer[i] = str[i];
    } else {
     buffer[i] = '-';
    }
  }
  buffer[i] = '\0';
  return buffer;
}

#undef DEFINE_STRUCT_INITIALIZER
#undef INITIALIZE_STRUCT
