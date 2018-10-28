all:
	racket dynamic-ffi/make.rkt
clean:
	rm -rf dynamic-ffi/compiled/native
