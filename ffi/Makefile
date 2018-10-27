all:
	raco ctool --xform dynamic-ffi.c
	raco ctool --3m --cc dynamic-ffi.3m.c 
	raco ctool --3m --ld \
	    compiled/native/x86_64-linux/3m/dynamic-ffi-core_rkt.so \
	    dynamic-ffi_3m.o \
	    ./header-parse.so
