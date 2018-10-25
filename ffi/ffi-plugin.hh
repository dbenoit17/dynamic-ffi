#ifndef DYNAMIC_FFI_PLUGIN_H
#define DYNAMIC_FFI_PLUGIN_H

#include <string>
#include <vector>
#include <set>
#include <iostream>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "llvm/Support/raw_ostream.h"

#include "header-parse.h"

using namespace clang;
using namespace clang::tooling;

/* begin plugin namespace */
namespace ffi {

class ffiAccumulator {
  std::vector<c_decl> declarations;
public:
  std::set<std::string> sources;
  bool deep_parse;
  int matches;
  ffiAccumulator(std::vector<std::string> &sources_vec) {
    this->matches = 0;
    sources = std::set<std::string>(sources_vec.begin(), sources_vec.end());
  };
  std::vector<c_decl> &get_c_decls() {return declarations;};
  void push_decl(c_decl d) {
    declarations.push_back(d);
  }
};

class ffiASTConsumer : public ASTConsumer {
private:
  ffiAccumulator &accumulator;
  CompilerInstance &compiler;
  bool deep_parse;
public:
  ffiASTConsumer(ffiAccumulator &accumulator,
     CompilerInstance &compiler, bool deep_parse)
   : accumulator(accumulator), compiler(compiler), deep_parse(deep_parse) {}
  /* overrideable handler methods
     https://clang.llvm.org/doxygen/classclang_1_1ASTConsumer.html
   */
  bool HandleTopLevelDecl(DeclGroupRef decls) override {
    for (DeclGroupRef::iterator i = decls.begin(),
         e = decls.end(); i != e; i++) {
      if (deep_parse || topLevelHeaderContains(*i)) {
        if (const VarDecl  *var_decl = dyn_cast<VarDecl>((Decl*) *i)) {
          c_decl d = make_global_var(var_decl);
          accumulator.push_decl(d);
        }
      }
    }
    return true;
  }
  bool topLevelHeaderContains(Decl *d) {
    std::string filename =
      compiler.getSourceManager().getFilename(d->getLocation()).str();
    return accumulator.sources.count(filename);
  }

  qualifiers get_quals(QualType type) {
    qualifiers quals;
    quals.is_const = type.isConstQualified();
    quals.is_volatile = type.isVolatileQualified();
    quals.is_restrict = type.isRestrictQualified();
    return quals;
  }

  c_decl make_global_var(const VarDecl *d) {
     QualType type = d->getType();

     std::string cxx_name = d->getNameAsString();
     std::string cxx_type = type.getAsString();
     char *name = (char*) malloc(sizeof(char*) * (cxx_name.length() + 1));
     char *type_name = (char*) malloc(sizeof(char*) * (cxx_type.length() + 1));
     strcpy(name, cxx_name.c_str());
     strcpy(type_name, cxx_type.c_str());

     qualifiers quals = get_quals(type);

     c_decl decl;
     if (type->isPointerType()) {
       decl = make_pointer_decl(name, type_name,
                  make_pointer_type(get_pointee_type(type), quals));
     } else {
       decl = make_global_var_decl(name, UNKNOWN, type_name, quals);
     }
     return decl;
  }
  c_type get_pointee_type(QualType type) {
    qualifiers quals = get_quals(type);
    if (type->isPointerType()) {
      QualType pointee = type->getPointeeType();
      return make_pointer_type(get_pointee_type(pointee), quals);
    }
    return make_atomic_c_type(UNKNOWN, quals);
  }
};

class ffiPluginAction : public ASTFrontendAction {
public:
  ffiPluginAction(ffiAccumulator &accumulator, bool deep_parse)
    : accumulator(accumulator), deep_parse(deep_parse) {}
protected:
  ffiAccumulator &accumulator;
  bool deep_parse;
  std::unique_ptr<ASTConsumer>
    CreateASTConsumer(CompilerInstance &compiler,
                      llvm::StringRef) override {
      return llvm::make_unique<ffiASTConsumer>(accumulator, compiler, deep_parse);
    }
};

template <typename T>
std::unique_ptr<FrontendActionFactory>
newFFIActionFactory(ffiAccumulator &accumulator, bool deep_parse) {
  class ffiActionFactory : public FrontendActionFactory {
    ffiAccumulator &accumulator;
    bool deep_parse;
  public:
    ffiActionFactory(ffiAccumulator &accumulator, bool deep_parse)
      : accumulator(accumulator), deep_parse(deep_parse) {}
    FrontendAction *create() override {
      return new T(accumulator, deep_parse);
    }
  };
  return std::unique_ptr<FrontendActionFactory>(new ffiActionFactory(accumulator, deep_parse));
}

} /* end plugin namespace */

#endif /* DYNAMIC_FFI_PLUGIN_H */
