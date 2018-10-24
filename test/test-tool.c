#include "../ffi/dynamic-ffi.h"

int main(int argc, const char **argv) {
  c_decl_array decls = ffi_parse(argc, argv);
  int i;
  printf("%d\n", decls.length);
  for (i = 0; i < decls.length; ++i) {
    print_decl(decls.data[i]);
  }
  free_decl_array(decls);
  //ffi_deep_parse(argc, argv);
  return 0;
}
