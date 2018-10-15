#ifndef DYNAMIC_FFI_H
#define DYNAMIC_FFI_H

#include <string>
#include <set>
#include <iostream>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::tooling;

/* begin plugin namespace */ 
namespace ffi {

typedef std::set<std::string> TemplateSet;

class ffiAccumulator {
public:
  int matches;
  ffiAccumulator() {this->matches = 0;};
};

class ffiVisitor : public RecursiveASTVisitor<ffiVisitor> {
private:
  TemplateSet templates;
public:
  ffiVisitor(TemplateSet &templates) : templates(templates) {}
  bool VisitFunctionDecl(FunctionDecl *FD) {
    return true;
  }
};

class ffiASTConsumer : public ASTConsumer {
private:
  CompilerInstance &compiler;
  TemplateSet templates;
  ffiAccumulator &accumulator;
public:
  ffiASTConsumer(CompilerInstance &compiler,
              TemplateSet templates, ffiAccumulator &accumulator)
     : compiler(compiler), templates(templates), accumulator(accumulator) {}
  /* overrideable handler methods
     https://clang.llvm.org/doxygen/classclang_1_1ASTConsumer.html 
   */
  bool HandleTopLevelDecl(DeclGroupRef decls) override {
    for (DeclGroupRef::iterator i = decls.begin(), 
         e = decls.end(); i != e; i++) {
      if (const FunctionDecl  *func_decl = dyn_cast<FunctionDecl>((Decl*) *i)) {
        if (const NamedDecl  *named_decl = dyn_cast<NamedDecl>((Decl*) *i)) {
          this->accumulator.matches++;
          std::cout << "function name: \"" << named_decl->getNameAsString() << "\"\n";
          std::cout << "   num params: " << func_decl->getNumParams() << "\n";
        }
      }
    }
    return true;
  }
};

class ffiPluginAction : public ASTFrontendAction {
public:
  ffiPluginAction(ffiAccumulator &accumulator) 
    : accumulator(accumulator){}
private:
  TemplateSet templates;
protected:
  ffiAccumulator &accumulator;
  std::unique_ptr<ASTConsumer> 
    CreateASTConsumer(CompilerInstance &compiler,
                      llvm::StringRef) override {
      return llvm::make_unique<ffiASTConsumer>(compiler, templates, accumulator);
    }
}; 

template <typename T>
std::unique_ptr<FrontendActionFactory> newFFIActionFactory(ffiAccumulator &accumulator) {
  class ffiActionFactory : public FrontendActionFactory {
    ffiAccumulator &accumulator;
  public:
    ffiActionFactory(ffiAccumulator &accumulator) : accumulator(accumulator) {}
    FrontendAction *create() override {
      return new T(accumulator);
    }
  };
  return std::unique_ptr<FrontendActionFactory>(new ffiActionFactory(accumulator));
}

} /* end plugin namespace */

#endif /* DYNAMIC_FFI_H */
