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

const char* decl_type_get_str(c_decl d) {
  switch (d.base) {
    case FUNCTION_DECL:
      return "FUNCTION_DECL";
      break;
    case GLOBAL_VAR_DECL:
      return "GLOBAL_VAR_DECL";
      break;
    case STRUCT_DECL:
      return "STRUCT_DECL";
      break;
    case UNION_DECL:
      return "UNION_DECL";
      break;
    default:
      printf("unknown c_decl type");
      exit(0);
  }
}

const char* c_type_get_str(c_type s) {
  switch (s.base) {
  case INTEGER:
    return "int";
    break;
    break;
  case STRUCT:
    return "struct";
    break;
  case UNION:
    return "union";
    break;
  case POINTER:
    return "pointer";
    break;
  case UNKNOWN:
  default:
    return "uknown";
    break;
  }
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

char* format_decl(c_decl d) {
  c_type *t = &(d.ctype);
  unsigned int size = 512;
  unsigned int length = 0;
  char *buffer = (char*) malloc(sizeof(char) * size);

  string_append(&buffer, decl_type_get_str(d), &length, &size);
  string_append(&buffer, "\n name: ", &length, &size);
  string_append(&buffer, d.name, &length, &size);
  string_append(&buffer, "\n type: ", &length, &size);

  if (!t->has_fields) {
    string_append(&buffer, c_type_get_str(*t), &length, &size);
  }
  while (t->has_fields) {
    switch (t->base) {
      case STRUCT:
        break;
      case UNION:
        break;
      case POINTER:
        {
          int indirects = 0, i;
          t = c_type_pointer_deref(t);
          while (t->base == POINTER) {
            ++indirects;
            t = c_type_pointer_deref(t);
          }
         string_append(&buffer, c_type_get_str(*t), &length, &size);
          for (i = 0; i < indirects; ++i) {
            string_append(&buffer, "*", &length, &size);
          }
        }
        break;
      default:
        break;
    }
  }
  return buffer;
}

void print_decl(c_decl d) {
  char* temp = format_decl(d);
  printf("%s\n", temp);
  free(temp);
}

} /* end extern C */
