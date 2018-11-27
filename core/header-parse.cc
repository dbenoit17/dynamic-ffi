#include <string>
#include <vector>
#include <set>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

#include "ffi-plugin.hh"
#include "header-parse.h"

using namespace clang::tooling;
using namespace clang;
using namespace llvm;
using namespace ffi;

static llvm::cl::OptionCategory MyToolCategory("my-tool options");

c_decl_array dynamic_ffi_parse(int argc, const char **argv, int deep_parse) {
  /* CommonOptionsParser has some external
     static caching going on which results
     in duplication/memory leaks of file header names.
     We must build our own compilation database
     and source list if we expect multiple calls
     to dynamic_ffi_parse(...) */
  FixedCompilationDatabase *DB =
    new FixedCompilationDatabase(".", std::vector<std::string>());
  std::vector<std::string> source_list;
  for (int i = 1; i < argc; ++i) {
    unsigned len = strlen(argv[i]);
    SmallString<sizeof(char)> st(argv[i], argv[i] + sizeof(char) *len);
    sys::fs::make_absolute(st);
    source_list.push_back(st.str());
  }

  ClangTool tool(*DB, (ArrayRef<std::string>) source_list);

  /* Accumulator object for gathered metadata.
     Should not contain the source list.
     It's just too convenient. */
  ffiAccumulator acc(source_list);

  // Run the tool
  int ret = tool.run(newFFIActionFactory<ffiPluginAction>(acc, deep_parse).get());
/*  if (! (ret || true)) {
    std::cout << "Dynamic ffi encountered an error.  Clang returned exit code: " << ret
         << "See Clang output for more details.  Exiting...\n";
    exit(ret);
  } */

  std::vector<c_decl> vdecls = acc.get_c_decls();
  c_decl * declarations = (c_decl*) malloc(sizeof(c_decl) * vdecls.size());
  std::copy(vdecls.begin(), vdecls.end(), declarations);

  // Cleanup
  delete DB;

  return { vdecls.size(), declarations };
}

extern "C" {

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

c_decl make_function_decl(char* name, c_type ctype, char* type_str) {
  c_decl d;
  d.name = name;
  d.base = FUNCTION_DECL;
  d.ctype = ctype;
  d.type_str = type_str;

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

c_type make_void_c_type(void) {
    c_type t;
    t.base = VOID;
    t.width = 0;
    t.is_const = 0;
    t.is_volatile = 0;
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

} /* end extern C */
