#ifndef DYNAMIC_FFI_H
#define DYNAMIC_FFI_H

typedef enum {
  ATOMIC,
  COMPOSITE
} type_size;

typedef enum {
  INT,
  INT16,
  INT32,
  INT64,
  UINT,
  UINT16,
  UINT32,
  UINT64,
  STRUCT,
  UNION,
} type_id;

typedef struct ctype {
  type_size size;
  union {
    struct {
      type_id id;
    } atomic;
    struct {
      type_id id;
      struct ctype * fields;
    } composite;
  } data;
} ctype;

typedef struct {
  char * name;
  ctype ret_type;
  ctype * arg_types;
} function_decl;

int ctool_wrapper(int argc, const char **argv);

#endif /* DYNAMIC_FFI_H */
