CC = clang++

LDFLAGS += -shared -Wl,-undefined,dynamic_lookup

CXXFLAGS += -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS \
	    -D__STDC_FORMAT_MACROS -Wno-long-long -fPIC    \
	    -fvisibility-inlines-hidden -fno-exceptions    \
	    -fno-rtti -std=c++11 -Wall

PLUGIN_SRC = ffiPlugin.cc
BUILT_PLUGIN = $(shell pwd)/ffi-plugin.so
TEST_SRC = test/test-prg.c

ffi-plugin.so:
	$(CC) -v $(PLUGIN_SRC) \
	      $(shell llvm-config --cxxflags --ldflags) \
	      -o $(BUILT_PLUGIN) $(CXXFLAGS) $(LDFLAGS)

test: ffi-plugin.so
	clang -std=c99 -c $(TEST_SRC) -Xclang -load \
	      -Xclang $(BUILT_PLUGIN) -Xclang -plugin \
	      -Xclang dynamic-ffi 

clean:
	rm $(BUILT_PLUGIN)
