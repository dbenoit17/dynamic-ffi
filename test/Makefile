CC = clang++

TOOL_FLAGS = -lclangAST -lclangASTMatchers -lclangAnalysis -lclangBasic -lclangDriver -lclangEdit -lclangFrontend -lclangFrontendTool -lclangLex -lclangParse -lclangSema -lclangEdit -lclangRewrite -lclangRewriteFrontend -lclangStaticAnalyzerFrontend -lclangStaticAnalyzerCheckers -lclangStaticAnalyzerCore -lclangSerialization -lclangToolingCore -lclangTooling -lclangFormat -lLLVM


LDFLAGS += -shared -Wl,-undefined,dynamic_lookup -fPIC

CXXFLAGS += -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS \
	    -D__STDC_FORMAT_MACROS -Wno-long-long -fPIC    \
	    -fvisibility-inlines-hidden -fno-exceptions    \
	    -fno-rtti -std=c++11 -Wall

TEST_SRC = test-prg.c
DYNAMIC_FFI_CC_SRC=../ffi/dynamic-ffi.cc
DYNAMIC_FFI_CC_O=./dynamic-ffi.cc.so
DYNAMIC_FFI_C_SRC=../ffi/dynamic-ffi.c
DYNAMIC_FFI_C_O=./dynamic-ffi.c.so



test-tool: dynamic-ffi.cc.so test-tool.so
	clang++ -v -g $(DYNAMIC_FFI_CC_O) ./test-tool.so -o test-tool

test-tool.so: test-tool.c
	clang -v -g test-tool.c -fPIC $(LDFLAGS) -o test-tool.so

dynamic-ffi.cc.so: $(DYNAMIC_FFI_CC_SRC)
	clang++ -v -g $(DYNAMIC_FFI_CC_SRC) $(CXXFLAGS)  $(TOOL_FLAGS) -o $(DYNAMIC_FFI_CC_O) $(shell llvm-config --cxxflags --ldflags --libs --libfiles --systemlibs) $(LDFLAGS)


clean:
	rm *.o *.so test-tool
 
