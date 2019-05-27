all:
	racket make.rkt
link:
	racket -e "(require \"make.rkt\") (raco-link)"

install:
	raco pkg install

reinstall:
	raco pkg remove dynamic-ffi
	raco pkg install


clean:
	rm -rf compiled/native
	rm -rf private/native

clean-cache:
	rm -rf compiled/ffi-cache

test: clean-cache
	raco test -p dynamic-ffi

typedef-test:
	racket make.rkt
	racket test/typedef-struct.rkt

