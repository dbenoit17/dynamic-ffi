#include "ffi-plugin.hh"

/* https://clang.llvm.org/doxygen/classclang_1_1Type.html
   https://clang.llvm.org/doxygen/classclang_1_1TargetInfo.html */

bool ffi::ffiASTConsumer::HandleTopLevelDecl(DeclGroupRef decls) {
  for (DeclGroupRef::iterator i = decls.begin(),
       e = decls.end(); i != e; i++) {
    if (deep_parse || topLevelHeaderContains(*i)) {
      if (const VarDecl  *var_decl = dyn_cast<VarDecl>((Decl*) *i)) {
        c_decl d = make_decl_from_global_var(var_decl);
        accumulator.push_decl(d);
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

c_decl ffi::ffiASTConsumer::make_decl_from_global_var(const Decl *dec) {
   const VarDecl  *d= dyn_cast<VarDecl>(dec);
   QualType type = d->getType();

   std::string cxx_name = d->getNameAsString();
   std::string cxx_type = type.getAsString();
   char *name = (char*) malloc(sizeof(char*) * (cxx_name.length() + 1));
   char *type_str= (char*) malloc(sizeof(char*) * (cxx_type.length() + 1));
   strcpy(name, cxx_name.c_str());
   strcpy(type_str, cxx_type.c_str());

   c_decl decl;

   if (type->isPointerType()) {
      int is_const = type.isConstQualified();
      int is_volatile = type.isVolatileQualified();
      int is_restrict = type.isRestrictQualified();
     decl = make_global_var_decl(name,
                make_pointer_c_type(get_pointee_type(type, dec),
                  is_const, is_volatile, is_restrict),
                type_str);
   } else {
     c_type ctype = dispatch_on_type(type, dec);
     decl = make_global_var_decl(name, ctype, type_str);
   }
   return decl;
}
c_type ffi::ffiASTConsumer::get_pointee_type(QualType type, const Decl* d) {
  if (type->isPointerType()) {
    QualType pointee = type->getPointeeType();
    int is_const = type.isConstQualified();
    int is_volatile = type.isVolatileQualified();
    int is_restrict = type.isRestrictQualified();
    return make_pointer_c_type(get_pointee_type(pointee, d), is_const, is_volatile, is_restrict);
  } else {
    return dispatch_on_type(type, d);
  }
}


c_type ffi::ffiASTConsumer::dispatch_on_type(QualType qual_type, const Decl *d) {
  const clang::Type * type = qual_type.getTypePtr();
  c_type ctype;
  int is_const = qual_type.isConstQualified();
  int is_volatile = qual_type.isVolatileQualified();
  int is_restrict = qual_type.isRestrictQualified();
  int is_signed;
  unsigned int width = d->getASTContext().getTypeInfo(qual_type).Width;
  c_type_id base_type;

  if (type->isIntegerType()) {
    base_type = INTEGER;
    if (type->hasSignedIntegerRepresentation()) {
      ctype = make_signed_int_c_type(width, is_const, is_volatile);
    }
    else if (type->hasUnsignedIntegerRepresentation()) {
      ctype = make_unsigned_int_c_type(width, is_const, is_volatile);
    }
    else {
      printf("signed int error");
    }
  }
  else if (type->isFloatingType()) {
    // more stuff
  }
  return ctype;
}

