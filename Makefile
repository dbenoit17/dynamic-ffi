all:
	racket make.rkt

reinstall:
	raco pkg remove dynamic-ffi
	raco pkg install


clean:
	rm -rf compiled/native
