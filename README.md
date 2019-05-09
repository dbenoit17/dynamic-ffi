dynamic-ffi
===========

Gather top-level declaration syntax-tree data from C
headers via clang.

This module is currently only available for GNU/Linux.

## Installation
To install the package:
```
raco pkg install https://github.com/dbenoit17/dynamic-ffi.git
```

### Dependencies:
* racket
* llvm and clang development headers and libraries

Fedora:
```
sudo dnf install '@development tools" racket llvm-devel clang-devel
```
Ubuntu:
```
sudo apt-get install 'build-essential" racket llvm-dev libclang-dev clang
```
