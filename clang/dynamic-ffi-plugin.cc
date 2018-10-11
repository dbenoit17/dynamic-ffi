#ifndef DYNAMIC_FFI_H
#define DYNAMIC_FFI_H

#include <string>
#include <set>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;

/* begin plugin namespace */ 
namespace {

typedef std::set<std::string> TemplateSet;

class ffiVisitor : public RecursiveASTVisitor<ffiVisitor> {
private:
  const TemplateSet &templates;
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
      if (const NamedDecl  *decl = dyn_cast<NamedDecl>((Decl*) *i)) {
        llvm::errs() << "top-level-decl: \"" << decl->getNameAsString() << "\"\n";
      }
      if (const FunctionDecl  *decl = dyn_cast<FunctionDecl>((Decl*) *i)) {
        llvm::errs() << "   num params: " << decl->getNumParams() << "\n";
      }
    }
    return true;
  }
};

class ffiPluginAction : public PluginASTAction {
private:
  TemplateSet templates;
protected:
  std::unique_ptr<ASTConsumer> 
    CreateASTConsumer(CompilerInstance &compiler,
                      llvm::StringRef) override {
      return llvm::make_unique<ffiASTConsumer>(compiler, templates);
    }
  bool ParseArgs(const CompilerInstance &CI,
                 const std::vector<std::string> &args) override {
    return true;
  }
  void PrintHelp(llvm::raw_ostream& out) {
    out << "You don't need help\n";
  }

}; 

} /* end plugin namespace */

static FrontendPluginRegistry::Add<ffiPluginAction>
X("dynamic-ffi", "provide data for dynamic ffi creation");

#endif /* DYNAMIC_FFI_H */
