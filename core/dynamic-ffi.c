/* This file provides a native extension to
   Racket for interfacing with the dynamic-ffi
   routines. */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#include "escheme.h"

#include "clang-export.h"
#include "wrap-fork.h"

const int struct_flags = SCHEME_STRUCT_NO_MAKE_PREFIX;
Scheme_Object *dynamic_ffi_parse(int argc, Scheme_Object ** argv);
Scheme_Object *make_decl_instance(c_decl *decl);

Scheme_Object *scheme_initialize(Scheme_Env *env) {
  Scheme_Env *mod_env;
  Scheme_Object * result;
  Scheme_Object * parse_prim;

  mod_env = scheme_primitive_module(scheme_intern_symbol("dynamic-ffi-core"), env);

  parse_prim = scheme_make_prim_w_arity(dynamic_ffi_parse, "dynamic-ffi-parse", 1, -1);
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
  c_decl_array decls;
  char ** argv;
  int i;

  argv = malloc(sizeof (char*) * argc);

  for (i = 0; i < argc; ++i) {
    argv[i] = SCHEME_BYTE_STR_VAL(scheme_argv[i]);
  }

  declarations = scheme_null;

  decls = fork_ffi_parse(argc, argv);

  for (i = 0; i < decls.length; ++i) {
    Scheme_Object *decl_scheme;
    c_decl* d;
    d  = &(decls.data[i]);
    decl_scheme = make_decl_instance(d);
    declarations = scheme_make_pair(decl_scheme, declarations);
  }

  free(argv);
  free_decl_array(decls);
  return declarations;
}

const char* INTEGER_SYM = "integer";
const char* FLOATING_SYM = "floating";
const char* POINTER_SYM = "pointer";
const char* STRUCT_SYM = "struct";
const char* UNION_SYM = "union";
const char* ARRAY_SYM = "array";
const char* FUNCTION_SYM = "function";
const char* VOID_SYM = "void";
char* ctype_to_str(c_type *t) {
  const char * sym;
  switch (t->base) {
    case INTEGER:
      sym = INTEGER_SYM;
      break;
    case FLOATING:
      sym = FLOATING_SYM;
      break;
    case POINTER:
      sym = POINTER_SYM;
      break;
    case VOID:
      sym = VOID_SYM;
      break;
    case STRUCT:
      sym = STRUCT_SYM;
      break;
    case UNION:
      sym = UNION_SYM;
      break;
    case ARRAY:
      sym = ARRAY_SYM;
      break;
    case FUNCTION:
      sym = FUNCTION_SYM;
      break;
    default:
      sym = "unknown";
      break;
  };
  return sym;
}

const char* FUNCTION_DECL_SYM = "function-decl";
const char* VAR_DECL_SYM = "var-decl";
const char* RECORD_DECL_SYM = "record-decl";
const char* ENUM_DECL_SYM = "enum-decl";
const char* TYPEDEF_DECL_SYM = "typedef-decl";
char* decl_to_str(c_decl *d) {
  const char * sym;
  switch (d->base) {
    case FUNCTION_DECL:
      sym = FUNCTION_DECL_SYM;
      break;
    case GLOBAL_VAR_DECL:
      sym = VAR_DECL_SYM;
      break;
    case RECORD_DECL:
      sym = RECORD_DECL_SYM;
      break;
    case ENUM_DECL:
      sym = ENUM_DECL_SYM;
      break;
    case TYPEDEF_DECL:
      sym = TYPEDEF_DECL_SYM;
      break;
    default:
      sym = "unknown-decl";
      break;
    }
  return sym;
}

Scheme_Object *make_ctype_instance(c_type *t) {
  Scheme_Object *new_ctype;
  Scheme_Object *is_const;
  Scheme_Object *is_volatile;
  Scheme_Object *is_restrict;
  Scheme_Object *is_signed;
  Scheme_Object *is_literal;
  Scheme_Object *width;
  Scheme_Object *sym;
  Scheme_Object *field_list;

  int i;

  is_const = t->is_const ? scheme_true : scheme_false;
  is_volatile = t->is_volatile ? scheme_true : scheme_false;
  is_restrict = t->is_restrict ? scheme_true : scheme_false;
  is_signed = t->is_signed ? scheme_true : scheme_false;
  is_literal = t->is_literal ? scheme_true : scheme_false;

  width = scheme_make_integer(t->width);

  sym = scheme_intern_symbol(ctype_to_str(t));
  field_list = scheme_null;

  for (i = t->field_length -1; i >= 0; --i) {
    Scheme_Object *new_field;

    new_field = make_ctype_instance(t->fields + i);
    field_list = scheme_make_pair(new_field, field_list);
  }

  new_ctype =
   scheme_make_pair(sym,
    scheme_make_pair(width,
     scheme_make_pair(is_signed,
      scheme_make_pair(is_const,
       scheme_make_pair(is_volatile,
        scheme_make_pair(is_restrict,
         scheme_make_pair(is_literal,
          scheme_make_pair(field_list,
           scheme_null))))))));
  return new_ctype;

}

Scheme_Object *make_decl_instance(c_decl *decl) {
  Scheme_Object * new_declaration;
  Scheme_Object * name;
  Scheme_Object * ctype;
  Scheme_Object * type_str;
  Scheme_Object * sym;
  Scheme_Object * literal_val;

  name = scheme_make_utf8_string(decl->name);
  type_str = scheme_make_utf8_string(decl->type_str);
  ctype = make_ctype_instance(&(decl->ctype));
  if (decl->base == ENUM_DECL) {
    int64_t *value;
    value = (int64_t*) decl->val;
    literal_val =  scheme_make_integer(*value);
  }
  else {
    literal_val = scheme_false;
  }

  sym = scheme_intern_symbol(decl_to_str(decl));

  new_declaration =
    scheme_make_pair(sym,
     scheme_make_pair(type_str,
      scheme_make_pair(name,
       scheme_make_pair(ctype,
         scheme_make_pair(literal_val, scheme_null)))));
  return new_declaration;
}

