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

/* https://clang.llvm.org/doxygen/classclang_1_1Type.html
   https://clang.llvm.org/doxygen/classclang_1_1TargetInfo.html */

//#define DYNAMIC_FFI_DEBUG

#ifdef DYNAMIC_FFI_DEBUG
  #define __debug(z)  z

#else
  #define __debug(z)
#endif

bool ffi::ffiASTConsumer::HandleTopLevelDecl(DeclGroupRef decls) {
  for (DeclGroupRef::iterator i = decls.begin(),
       e = decls.end(); i != e; i++) {

    if (deep_parse || topLevelHeaderContains(*i)) {
      if (const VarDecl  *var_decl = dyn_cast<VarDecl>((Decl*) *i)) {
        c_decl d = make_decl_from_global_var(var_decl);
        accumulator.push_decl(d);
      }
      else if (const EnumDecl  *enum_decl = dyn_cast<EnumDecl>((Decl*) *i)) {
        c_type ctype = dispatch_on_type(enum_decl->getIntegerType(), enum_decl);
        for (auto j = enum_decl->enumerator_begin(); j != enum_decl->enumerator_end(); j++){
          c_decl d = make_decl_from_enum_constant(*j);
          d.ctype = ctype;
          accumulator.push_decl(d);
        }
      }
      else if (const RecordDecl  *record_decl = dyn_cast<RecordDecl>((Decl*) *i)) {
        c_decl d = make_decl_from_record(record_decl);
        accumulator.push_decl(d);
      }

      else if (const FunctionDecl  *function_decl = dyn_cast<FunctionDecl>((Decl*) *i)) {
        c_decl d = make_decl_from_function(function_decl);
        accumulator.push_decl(d);
      }
      else if (const TypedefDecl  *typedef_decl = dyn_cast<TypedefDecl>((Decl*) *i)) {
        //c_decl d = make_decl_from_typedef(typedef_decl);
        //accumulator.push_decl(d);
      }
      else {
        fprintf(stderr, "\ndynamic-ffi: unimplemented decl at: \n");
        (*i)->dump();

      }
    }
  }
  return true;
}

bool ffi::ffiASTConsumer::topLevelHeaderContains(Decl *d) {
  std::string filename =
    compiler.getSourceManager().getFilename(d->getLocation()).str();
  return accumulator.sources.count(filename);
}

c_decl ffi::ffiASTConsumer::make_decl_from_enum_constant(const Decl *dec) {
   const EnumConstantDecl  *d = dyn_cast<EnumConstantDecl>(dec);

   std::string cxx_name = d->getNameAsString();
   char* st = "enum";
   char *name = (char*) malloc(sizeof(char*) * (cxx_name.length() + 1));
   char *type_str = (char*) malloc(sizeof(char*) * (strlen(st)+ 1));
   strcpy(name, cxx_name.c_str());
   strcpy(type_str, st);

   int64_t * value = (int64_t*) malloc(sizeof(int64_t));

   *value = d->getInitVal().getExtValue();
   c_type ctype;


   return make_enum_decl(name, ctype, type_str, value);
}

c_decl ffi::ffiASTConsumer::make_decl_from_function(const Decl *dec) {
   const FunctionDecl  *d = dyn_cast<FunctionDecl>(dec);

   std::string cxx_name = d->getNameAsString();
   char* st = "function";
   char *name = (char*) malloc(sizeof(char*) * (cxx_name.length() + 1));
   char *type_str = (char*) malloc(sizeof(char*) * (strlen(st)+ 1));
   strcpy(name, cxx_name.c_str());
   strcpy(type_str, st);
   __debug(fprintf(stderr, "%s\n", name);)

   c_type ctype = dispatch_on_type(d->getType(), dec);

   return make_function_decl(name, ctype, type_str, NULL);
}

c_decl ffi::ffiASTConsumer::make_decl_from_record(const Decl *dec) {
   const RecordDecl  *d = dyn_cast<RecordDecl>(dec);

   std::string cxx_name = d->getNameAsString();
   char* st = "struct";
   char *name = (char*) malloc(sizeof(char*) * (cxx_name.length() + 1));
   char *type_str = (char*) malloc(sizeof(char*) * (strlen(st)+ 1));
   strcpy(name, cxx_name.c_str());
   strcpy(type_str, st);

   const RecordType * rt = d->getTypeForDecl()->getAs<clang::RecordType>();
   c_type ctype = dispatch_on_type(rt->getCanonicalTypeUnqualified(), dec);

   return make_record_decl(name, ctype, type_str, NULL);
}

c_decl ffi::ffiASTConsumer::make_decl_from_global_var(const Decl *dec) {
   const VarDecl  *d= dyn_cast<VarDecl>(dec);
   QualType type = d->getType();

   std::string cxx_name = d->getNameAsString();
   std::string cxx_type = type.getAsString();
   char *name = (char*) malloc(sizeof(char*) * (cxx_name.length() + 1));
   char *type_str= (char*) malloc(sizeof(char*) * (cxx_type.length() + 1));
   strcpy(name, cxx_name.c_str());
   strcpy(type_str, cxx_type.c_str());

   c_type ctype = dispatch_on_type(type, dec);
   return make_global_var_decl(name, ctype, type_str, NULL);
}

c_type ffi::ffiASTConsumer::dispatch_on_type(QualType qual_type, const Decl *d) {
  //const clang::Type * type = qual_type.getTypePtr();
  const clang::Type * type = qual_type.getTypePtr()->getUnqualifiedDesugaredType();
  c_type ctype;
  int is_const = qual_type.isConstQualified();
  int is_volatile = qual_type.isVolatileQualified();
  int is_restrict = qual_type.isRestrictQualified();
  int is_signed;

  c_type_id base_type;
  if (type->isRecordType()) {
    __debug(type->dump());

    uint64_t width = 0;

    int field_length = 0;
    RecordDecl *rd = type->getAs<clang::RecordType>()->getDecl();
    for (auto i = rd->field_begin(); i != rd->field_end(); i++, field_length++);

    c_type* fields = (c_type*) malloc(sizeof(c_type) * field_length);
    for (auto i = rd->field_begin(); i != rd->field_end(); i++) {
      QualType field_type = i->getType();
      uint64_t field_width = rd->getASTContext().getTypeInfo(field_type).Width;
      fields[i->getFieldIndex()] = dispatch_on_type(field_type, rd);
      width += field_width;
    }
    if (type->isStructureType()) {
      ctype = make_struct_type(fields, field_length, 0, 0, width);
    }
    else if (type->isUnionType()) {
      ctype = make_union_type(fields, field_length, 0, 0, width);
    }
    else {
      fprintf(stderr, "record type error");
      exit(0);
    }
  }
  else {
   uint64_t width = d->getASTContext().getTypeInfo(type).Width;

   if (type->isIntegerType()) {
     __debug(type->dump());
     base_type = INTEGER;
     if (type->hasSignedIntegerRepresentation()) {
       ctype = make_signed_int_c_type(width, is_const, is_volatile);
     }
     else if (type->hasUnsignedIntegerRepresentation()) {
       ctype = make_unsigned_int_c_type(width, is_const, is_volatile);
     }
     else {
       fprintf(stderr, "int type error");
       exit(0);
     }
   }
   else if (type->isFloatingType()) {
     // more stuff
     __debug(type->dump());
     ctype = make_floating_c_type(width, is_const, is_volatile);
   }
   else if (type->isPointerType()) {
     __debug(type->dump());
     QualType pointee = type->getPointeeType();
     const clang::Type * pt = pointee.getTypePtr()->getUnqualifiedDesugaredType();
     if (pt->isRecordType()) {
       __debug(pt->dump());
       RecordDecl *rd = pointee->getAs<clang::RecordType>()->getDecl();
       uint64_t width = 0;
       for (auto i = rd->field_begin(); i != rd->field_end(); i++) {
         QualType field_type = i->getType();
         uint64_t field_width = rd->getASTContext().getTypeInfo(field_type).Width;
         width += field_width;
       }
       int p_is_const = pointee.isConstQualified();
       int p_is_volatile = pointee.isVolatileQualified();
       ctype = make_pointer_c_type(make_void_c_type(width, p_is_const, p_is_volatile),
                   is_const, is_volatile, is_restrict, width);
     }
     else {
       ctype = make_pointer_c_type(dispatch_on_type(pointee, d), is_const, is_volatile, is_restrict, width);
     }
   }

   else if (type->isArrayType()) {
     __debug(type->dump());
     QualType pointee = type->getAsArrayTypeUnsafe()->getElementType();
     const clang::Type * pt = pointee.getTypePtr()->getUnqualifiedDesugaredType();
     if (pt->isRecordType()) {
       __debug(pt->dump());
       RecordDecl *rd = pointee->getAs<clang::RecordType>()->getDecl();
       uint64_t width = 0;
       for (auto i = rd->field_begin(); i != rd->field_end(); i++) {
         QualType field_type = i->getType();
         uint64_t field_width = rd->getASTContext().getTypeInfo(field_type).Width;
         width += field_width;
       }
       int p_is_const = pointee.isConstQualified();
       int p_is_volatile = pointee.isVolatileQualified();
       ctype = make_array_c_type(make_void_c_type(width, p_is_const, p_is_volatile),
                   is_const, is_volatile, is_restrict, width);
     }
     else {
       ctype = make_array_c_type(dispatch_on_type(pointee, d), is_const, is_volatile, is_restrict, width);
     }
   }
   else if (type->isVoidType()) {
     __debug(type->dump());
     ctype = make_void_c_type(0,0,0);
   }

   else if (type->isFunctionProtoType()) {
     __debug(type->dump());
    const FunctionProtoType *fptype = type->getAs<clang::FunctionProtoType>();
    int field_length = fptype->getNumParams() + 1;;

    c_type* fields = (c_type*) malloc(sizeof(c_type) * field_length);
    fields[0] = dispatch_on_type(fptype->getReturnType(), d);
    for (int i = 0; i < field_length - 1; ++i) {
      QualType field_type = fptype->getParamTypes()[i];
      fields[i + 1] = dispatch_on_type(field_type, d);
    }
    ctype = make_function_type(fields, field_length);
   }
   else {
     fprintf(stderr, "\ndynamic-ffi: unimplemented type: %s: %s at:\n", type->getTypeClassName(), qual_type.getAsString().c_str());
     d->dump();
     ctype = make_unknown_c_type(width, is_const, is_volatile);
   }
  }
  return ctype;
}

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
  fprintf(stderr, "parse complete\n");

  std::vector<c_decl> vdecls = acc.get_c_decls();
  c_decl * declarations = (c_decl*) malloc(sizeof(c_decl) * vdecls.size());
  std::copy(vdecls.begin(), vdecls.end(), declarations);

  // Cleanup
  delete DB;

  return { vdecls.size(), declarations };
}

