#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>


#include "escheme.h"
#include "header-parse.h"

const int struct_flags = SCHEME_STRUCT_NO_MAKE_PREFIX;
Scheme_Object *dynamic_ffi_parse(int argc, Scheme_Object ** argv);
Scheme_Object *make_decl_instance(c_decl *decl);

Scheme_Object *scheme_initialize(Scheme_Env *env) {
  Scheme_Env *mod_env;
  Scheme_Object * result;
  Scheme_Object * parse_prim;

  mod_env = scheme_primitive_module(scheme_intern_symbol("dynamic-ffi-core"), env);


  //scheme_debug_print(scheme_make_utf8_string("parsing complete\n"));

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
  decls = ffi_parse(argc, argv);


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


Scheme_Object *make_atomic_ctype_instance(c_type *t) {
  Scheme_Object *new_ctype;
  Scheme_Object *is_const;
  Scheme_Object *is_volatile;
  Scheme_Object *is_restrict;
  Scheme_Object *width;
  const char * s;
  int x;

  x = t->width;
  s = c_type_get_str(*t);
  // make bools
  is_const = t->quals.is_const ? scheme_true : scheme_false;
  is_volatile = t->quals.is_volatile ? scheme_true : scheme_false;
  is_restrict = t->quals.is_restrict ? scheme_true : scheme_false;
  width = scheme_make_integer(x);

  new_ctype =
   scheme_make_pair(width,
     scheme_make_pair(is_const,
       scheme_make_pair(is_volatile,
         scheme_make_pair(is_restrict, scheme_null))));
  return new_ctype;

}

Scheme_Object *make_decl_instance(c_decl *decl) {
  Scheme_Object * new_declaration;
  Scheme_Object * name;
  Scheme_Object * type_info;
  Scheme_Object * qual_type;
  Scheme_Object * type;

  name = scheme_make_utf8_string(decl->name);
  type_info = make_atomic_ctype_instance(&(decl->type_info));
  qual_type = scheme_make_utf8_string(decl->qual_type);

  new_declaration =
    scheme_make_pair(qual_type,
      scheme_make_pair(name,
        scheme_make_pair( type_info, scheme_null)));
  return new_declaration;
}



