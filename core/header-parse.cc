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

c_type make_atomic_c_type(c_type_id tid, qualifiers quals) {
  c_type t;
  t.type_size = ATOMIC;
  t.data.atomic.id = tid;
  t.data.atomic.quals = quals;
  return  t;
}

c_type make_composite_c_type(c_type_id tid, unsigned int field_length,
                           c_type *fields) {
  c_type t;
  t.type_size = COMPOSITE;
  t.data.composite.id = tid;
  t.data.composite.fields = fields;
  t.data.composite.field_length = field_length;
  return  t;
}

c_type make_pointer_type(c_type type) {
  c_type t;
  t.type_size = COMPOSITE;
  t.data.composite.id = POINTER;
  t.data.composite.fields = (c_type*) malloc(sizeof(c_type));
  memcpy(t.data.composite.fields, &type, sizeof(c_type));
  t.data.composite.field_length = 1;
  return t;
}

c_decl make_global_var_decl(char *name, c_type_id tid,
                          char *qual_type,
                          qualifiers quals) {
  c_decl d;
  d.name = name;
  d.decl_type = GLOBAL_VAR_DECL;
  d.type_info = make_atomic_c_type(tid, quals);
  d.qual_type = qual_type;
  return d;
}

c_decl make_pointer_decl(char *name, char *qual_type, c_type type) {
  c_decl d;
  d.name = name;
  d.decl_type = GLOBAL_VAR_DECL;
  d.type_info = type;
  d.qual_type = qual_type;
  return d;
}

const char* c_type_get_size_str(c_type s) {
  switch (s.type_size) {
  case ATOMIC:
    return "ATOMIC";
    break;
  case COMPOSITE:
    return "COMPOSITE";
    break;
    default:
      printf("unknown type_size c_type");
      exit(0);
  }
}

const char* decl_type_get_str(c_decl d) {
  switch (d.decl_type) {
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
  switch (s.data.atomic.id) {
  case INT16:
    return "INT16";
    break;
  case INT32:
    return "INT32";
    break;
  case INT64:
    return "INT64";
    break;
  case UINT16:
    return "UINT16";
    break;
  case UINT32:
    return "UINT32";
    break;
  case UINT64:
    return "UINT64";
    break;
  case STRUCT:
    return "STRUCT";
    break;
  case UNION:
    return "UNION";
    break;
  case POINTER:
    return "POINTER";
    break;
  case UNKNOWN:
    return "UNKNOWN";
    break;
  default:
    printf("unknown c_type");
    exit(0);
  }
}

void c_type_free_field(c_type *t) {
  if (c_type_get_size(t) == COMPOSITE) {
    c_type *fields = c_type_get_fields(t);
    unsigned int length = c_type_get_field_length(t);
    unsigned int i;
    for (i = 0; i < length; ++i) {
      c_type_free_field(&(fields[i]));
    }
    free(fields);
  }
}

void free_decl(c_decl d) {
  c_type * t = &(d.type_info);
  free(d.name);
  free(d.qual_type);
  c_type_free_field(t);
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
  c_type *t = &(d.type_info);
  unsigned int size = 512;
  unsigned int length = 0;
  char *buffer = (char*) malloc(sizeof(char) * size);

  string_append(&buffer, decl_type_get_str(d), &length, &size);
  string_append(&buffer, "\n name: ", &length, &size);
  string_append(&buffer, d.name, &length, &size);
  string_append(&buffer, "\n type_size: ", &length, &size);
  string_append(&buffer, c_type_get_size_str(*t), &length, &size);
  string_append(&buffer, "\n type: ", &length, &size);

  if (c_type_get_size(t) == ATOMIC) {
    string_append(&buffer, c_type_get_str(*t), &length, &size);
  }
  while (c_type_get_size(t) == COMPOSITE) {
    switch (c_type_get_id(t)) {
      case STRUCT:
        break;
      case UNION:
        break;
      case POINTER:
        {
          int indirects = 0, i;
          t = c_type_pointer_deref(t);
          while (c_type_get_id(t) == POINTER) {
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
