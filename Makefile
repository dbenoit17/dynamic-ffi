all:
	racket make.rkt

reinstall:
	raco pkg remove dynamic-ffi
	raco pkg install


clean:
	rm -rf compiled/native

clean-cache:
	rm -rf compiled/ffi-cache

test: clean-cache reinstall
	raco test -p dynamic-ffi
