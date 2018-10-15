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
#include "llvm/Support/raw_ostream.h"

using namespace clang;

/* begin plugin namespace */ 
namespace ffi {

typedef std::set<std::string> TemplateSet;

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
public:
  ffiASTConsumer(CompilerInstance &compiler, 
              TemplateSet templates)
     : compiler(compiler), templates(templates) {}
  /* overrideable handler methods
     https://clang.llvm.org/doxygen/classclang_1_1ASTConsumer.html 
   */
  bool HandleTopLevelDecl(DeclGroupRef decls) override {
    for (DeclGroupRef::iterator i = decls.begin(), 
         e = decls.end(); i != e; i++) {
      if (const FunctionDecl  *func_decl = dyn_cast<FunctionDecl>((Decl*) *i)) {
        if (const NamedDecl  *named_decl = dyn_cast<NamedDecl>((Decl*) *i)) {
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
  ffiPluginAction() {this->matches = 0;}
private:
  TemplateSet templates;
  unsigned long long matches;
protected:
  std::unique_ptr<ASTConsumer> 
    CreateASTConsumer(CompilerInstance &compiler,
                      llvm::StringRef) override {
      return llvm::make_unique<ffiASTConsumer>(compiler, templates);
    }
}; 

} /* end plugin namespace */

#endif /* DYNAMIC_FFI_H */
