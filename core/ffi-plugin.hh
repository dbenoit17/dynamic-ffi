#ifndef DYNAMIC_FFI_PLUGIN_H
#define DYNAMIC_FFI_PLUGIN_H

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

#include "header-parse.h"

using namespace clang::tooling;
using namespace clang;
using namespace llvm;

/* begin plugin namespace */
namespace ffi {

class ffiAccumulator {
  std::vector<c_decl> declarations;
public:
  std::set<std::string> sources;
  bool deep_parse;
  ffiAccumulator(std::vector<std::string> &sources_vec) {
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
  bool HandleTopLevelDecl(DeclGroupRef decls) override;
  bool topLevelHeaderContains(Decl *d);
  c_decl make_decl_from_global_var(const Decl *d);
  c_decl make_decl_from_record(const Decl *dec);
  c_type get_pointee_type(QualType type, const Decl* d);

  c_type_id get_c_type_id(QualType type);
  c_type dispatch_on_type(QualType qual_type, const Decl *d);
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
