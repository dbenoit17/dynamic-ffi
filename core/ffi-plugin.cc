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
      else if (const RecordDecl  *record_decl = dyn_cast<RecordDecl>((Decl*) *i)) {
        c_decl d = make_decl_from_record(record_decl);
        accumulator.push_decl(d);
      }
      else if (const FunctionDecl  *function_decl = dyn_cast<FunctionDecl>((Decl*) *i)) {
        c_decl d = make_decl_from_function(function_decl);
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

c_decl ffi::ffiASTConsumer::make_decl_from_function(const Decl *dec) {
   const FunctionDecl  *d = dyn_cast<FunctionDecl>(dec);

   std::string cxx_name = d->getNameAsString();
   char* st = "function";
   char *name = (char*) malloc(sizeof(char*) * (cxx_name.length() + 1));
   char *type_str = (char*) malloc(sizeof(char*) * (strlen(st)+ 1));
   strcpy(name, cxx_name.c_str());
   strcpy(type_str, st);

   int field_length = d->getNumParams() + 1;;

   c_type* fields = (c_type*) malloc(sizeof(c_type) * field_length);
   fields[0] = dispatch_on_type(d->getReturnType(), d);
   for (int i = 0; i < field_length - 1; ++i) {
     QualType field_type = d->parameters()[i]->getType();
     fields[i + 1] = dispatch_on_type(field_type, dec);
   }
   c_type ctype = make_function_type(fields, field_length);

   return make_function_decl(name, ctype, type_str);
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

   return make_struct_decl(name, ctype, type_str);
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
   return make_global_var_decl(name, ctype, type_str);
}

c_type ffi::ffiASTConsumer::dispatch_on_type(QualType qual_type, const Decl *d) {
  const clang::Type * type = qual_type.getTypePtr();
  c_type ctype;
  int is_const = qual_type.isConstQualified();
  int is_volatile = qual_type.isVolatileQualified();
  int is_restrict = qual_type.isRestrictQualified();
  int is_signed;
  uint64_t width = d->getASTContext().getTypeInfo(qual_type).Width;
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
      printf("int error");
      exit(0);
    }
  }
  else if (type->isFloatingType()) {
    // more stuff
    ctype = make_floating_c_type(width, is_const, is_volatile);
  }
  else if (type->isPointerType()) {
    QualType pointee = type->getPointeeType();
    ctype = make_pointer_c_type(dispatch_on_type(pointee, d), is_const, is_volatile, is_restrict, width);
  }

  else if (type->isArrayType()) {
  //  printf("record type: %s\n", qual_type.getAsString().c_str());
    QualType pointee = type->getAsArrayTypeUnsafe()->getElementType();
    ctype = make_array_c_type(dispatch_on_type(pointee, d), is_const, is_volatile, is_restrict, width);
  }
  else if (type->isVoidType()) {
  //  printf("record type: %s\n", qual_type.getAsString().c_str());
    QualType pointee = type->getAsArrayTypeUnsafe()->getElementType();
    ctype = make_void_c_type();
  }
  else if (type->isRecordType()) {
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
   ctype = make_struct_type(fields, field_length, 0, 0, width);
  }

  else {
    printf("unknown type: %s\n", qual_type.getAsString().c_str());
    ctype = make_unknown_c_type(width, is_const, is_volatile);
  }
  return ctype;
}

