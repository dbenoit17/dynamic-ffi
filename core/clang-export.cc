#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
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

#include "clang-plugin.hh"
#include "clang-export.h"

using namespace clang::tooling;
using namespace clang;
using namespace llvm;
using namespace ffi;

static llvm::cl::OptionCategory MyToolCategory("my-tool options");

c_decl_array dynamic_ffi_parse(size_t gc_mem_limit, dhgc_block *shm_block, int argc, const char **argv, int deep_parse) {

  c_decl **declarations = (c_decl**) dhgc_alloc(shm_block, sizeof(c_decl*));
  uint64_t * decls_length = (uint64_t*) dhgc_alloc(shm_block, sizeof(uint64_t*));

/* Clang libs seem to make lots of assumptions
   that they own the entire process space.
   It seems like the libraries are designed
   with the idea that they would generally
   not be used in an interactive context.
   Linking with clang seems to cause major
   tty issues.  Forking the clang procedures
   into their own process space is the best
   solution so far. */

  pid_t pid;
  pid = fork();
  if (pid < 0) {
    printf("could not fork\n");
  }
  else if (pid == 0) {
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

      ffiAccumulator acc(source_list);

      std::vector<c_decl> vdecls;
      tool.run(newFFIActionFactory<ffiPluginAction>(acc, deep_parse, shm_block).get());
      vdecls = acc.get_c_decls();
      *decls_length = vdecls.size();
      *declarations = (c_decl*) dhgc_alloc(shm_block, sizeof(c_decl) * (*decls_length));
      std::copy(vdecls.begin(), vdecls.end(), *declarations);
      delete DB;
      exit(0);
  }
  else {
    wait(NULL);
  }

  // Cleanup

  return { *decls_length, *declarations };
}

extern "C" {

// Parse only the headers specified in argv[1:]
c_decl_array ffi_parse(size_t gc_mem_limit, dhgc_block *shm_block, int argc, const char **argv) {
  return dynamic_ffi_parse(gc_mem_limit, shm_block, argc, argv, false);
}

// Parse nested includes too
c_decl_array ffi_deep_parse(size_t gc_mem_limit, dhgc_block *shm_block, int argc, const char **argv) {
  return dynamic_ffi_parse(gc_mem_limit, shm_block, argc, argv, true);
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

c_type make_pointer_c_type(c_type * type, int is_const, int is_volatile, int is_restrict, uint64_t width) {
  c_type t;
  t.base = POINTER;
  t.fields = type;
  t.has_fields = 1;
  t.field_length = 1;
  t.is_signed = 0;
  t.is_literal = 0;
  t.is_const = is_const;
  t.is_volatile = is_volatile;
  t.is_restrict = is_restrict;
  t.width = width;
  return t;
}
c_type make_array_c_type(c_type * type, int is_const, int is_volatile, int is_restrict, uint64_t width) {
  c_type t;
  t.base = ARRAY;
  t.fields = type;
  t.has_fields = 1;
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

} /* end extern C */
