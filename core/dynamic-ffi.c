#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>


#include "escheme.h"
#include "header-parse.h"

/*
   DEFINE_STRUCT_INITIALIZER(...)
   Create an initializer for a racket struct;
   NAME: id
   SUPER: id of parent
   NUM_FIELDS: int
   FIELDS ...: const char*
*/
#define DEFINE_STRUCT_INITIALIZER(NAME, SUPER, NUM_FIELDS, FIELDS ...) \
const char* struct_##NAME##_fields[NUM_FIELDS] = {FIELDS}; \
Scheme_Object * NAME; \
void struct_##NAME##_init(Scheme_Env * env) { \
  Scheme_Object *lst; \
  Scheme_Object *name_symbol; \
  Scheme_Object *properties; \
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
  lst = EMPTY_LIST(); \
  NAME = scheme_make_struct_type(name_symbol, SUPER, NULL, NUM_FIELDS, 0, NULL, lst, NULL); \
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
Scheme_Object *dynamic_ffi_parse(int argc, Scheme_Object ** argv);
Scheme_Object *make_decl_instance(c_decl *decl);
char *dashify(const char *str);

static inline Scheme_Object *CONS(Scheme_Object *car, Scheme_Object *cdr) {
  return scheme_make_pair(car, cdr);
}

static inline Scheme_Object *EMPTY_LIST() {
  return scheme_build_list(0,NULL);
}

DEFINE_STRUCT_INITIALIZER(declaration, NULL, 3,
                          "name", "type", "type-string");
DEFINE_STRUCT_INITIALIZER(func_decl, declaration, 0);
DEFINE_STRUCT_INITIALIZER(var_decl, declaration, 0);
DEFINE_STRUCT_INITIALIZER(struct_decl, declaration, 0);
DEFINE_STRUCT_INITIALIZER(union_decl, declaration, 0);

DEFINE_STRUCT_INITIALIZER(ctype, NULL, 0);
DEFINE_STRUCT_INITIALIZER(ctype_atomic, ctype, 3,
                          "const?", "volatile?", "restrict?");

DEFINE_STRUCT_INITIALIZER(ctype_int, ctype_atomic, 1, "width");
DEFINE_STRUCT_INITIALIZER(ctype_signed_int, ctype_int, 0);
DEFINE_STRUCT_INITIALIZER(ctype_unsigned_int, ctype_int, 0);

DEFINE_STRUCT_INITIALIZER(ctype_float, ctype_atomic, 1, "width");
DEFINE_STRUCT_INITIALIZER(ctype_signed_float, ctype_float, 0);
DEFINE_STRUCT_INITIALIZER(ctype_unsigned_float, ctype_float, 0);

DEFINE_STRUCT_INITIALIZER(ctype_composite, ctype, 0);
DEFINE_STRUCT_INITIALIZER(ctype_struct, ctype_composite, 0);
DEFINE_STRUCT_INITIALIZER(ctype_union, ctype_composite, 0);
DEFINE_STRUCT_INITIALIZER(ctype_pointer, ctype_composite, 0);
DEFINE_STRUCT_INITIALIZER(ctype_function, ctype_composite, 0);

Scheme_Object *scheme_initialize(Scheme_Env *env) {
  Scheme_Env *mod_env;
  Scheme_Object * result;
  Scheme_Object * parse_prim;

  mod_env = scheme_primitive_module(scheme_intern_symbol("dynamic-ffi-core"), env);

  parse_prim = scheme_make_prim_w_arity(dynamic_ffi_parse, "dynamic-ffi-parse", 1, -1);

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
  INITIALIZE_STRUCT(ctype_unsigned_int, mod_env);

  INITIALIZE_STRUCT(ctype_float, mod_env);
  INITIALIZE_STRUCT(ctype_signed_float, mod_env);
  INITIALIZE_STRUCT(ctype_unsigned_float, mod_env);

  INITIALIZE_STRUCT(ctype_composite, mod_env);
  INITIALIZE_STRUCT(ctype_struct, mod_env);
  INITIALIZE_STRUCT(ctype_union, mod_env);
  INITIALIZE_STRUCT(ctype_pointer, mod_env);
  INITIALIZE_STRUCT(ctype_function, mod_env);

  scheme_add_global("dynamic-ffi-parse", parse_prim, mod_env);

  scheme_finish_primitive_module(mod_env);
  return scheme_void;
}

Scheme_Object *scheme_reload(Scheme_Env *env) {
  return scheme_initialize(env);
}

Scheme_Object *scheme_module_name() {
  return scheme_intern_symbol("dynamic-ffi-core");
}

Scheme_Object *dynamic_ffi_parse(int argc, Scheme_Object **scheme_argv) {
  Scheme_Object *declarations;
  char **argv;
  int i;
  c_decl_array decls;

  argv = malloc(sizeof(char*) * (argc + 1));

  argv[0] = "dynamic-ffi-parse";

  for (i = 0; i < argc; i++) {
    unsigned int length, j;
    mzchar* temp;
    temp = SCHEME_CHAR_STR_VAL(scheme_argv[i]);
    length = SCHEME_CHAR_STRLEN_VAL(scheme_argv[i]);
    argv[i+1] = malloc(sizeof(char) * (length + 1));
    for (j = 0; j < length; j++) {
       mzchar mz;
       mz = temp[j];
       if (mz >= 256) {
         printf("only ascii paths supported currently");
       }
       argv[i+1][j] = (char) mz;
    }
    argv[i+1][j] = '\0';
  }

  declarations = EMPTY_LIST();

  decls = ffi_parse(argc + 1, argv);

  for (i = 0; i < decls.length; ++i) {
    Scheme_Object *decl_scheme;
    c_decl* d;
    d  = decls.data + i;
    decl_scheme = make_decl_instance(d);
    declarations = CONS(decl_scheme, declarations);
  }

  free_decl_array(decls);
  for (i = 0; i < argc; ++i) {
    free(argv[i+1]);
  }
  free(argv);
  return declarations;
}


Scheme_Object *make_atomic_ctype_instance(c_type *t) {
  Scheme_Object *new_ctype;
  Scheme_Object *argv[3];
  const char * s;

  s = c_type_get_str(*t);
  // make bools
  argv[0] = scheme_make_utf8_string(s);
  argv[1] = scheme_make_utf8_string(s);
  argv[2] = scheme_make_utf8_string(s);

  new_ctype = scheme_make_struct_instance(ctype_atomic, 3, argv);
  return new_ctype;

}

Scheme_Object *make_decl_instance(c_decl *decl) {
  Scheme_Object * new_declaration;
  Scheme_Object * argv[3];
  Scheme_Object * type;

  argv[0] = scheme_make_utf8_string(decl->name);
  argv[1] = make_atomic_ctype_instance(&(decl->type_info));
  argv[2] = scheme_make_utf8_string(decl->qual_type);

  new_declaration =
    scheme_make_struct_instance(declaration, 3, argv);
  return new_declaration;
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

