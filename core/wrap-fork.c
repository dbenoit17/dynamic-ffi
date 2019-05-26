/* Linking with clang libraries from within an
   interactive tty process causes issues with
   the tty display.  This seems to be an issue
   within the process space, because forking
   the clang linkage to a subprocess causes the
   issue to dissappear.

   This file implements a workaround to the clang
   tty issue.  The call to ffi_parse() runs in a
   child process, and the C declarations are
   synchronously piped back to the parent (racket). */

#include <pthread.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/wait.h>

#include "wrap-fork.h"

extern "C" {


c_decl read_decl(int *pipe_ready, int fdin, pthread_mutex_t *mutex,
                 pthread_cond_t *cond);
void write_decl(int *pipe_ready, int fdout, c_decl old_decl,
                pthread_mutex_t *mutex, pthread_cond_t *cond);
c_type read_ctype(int *pipe_ready, int fdin, pthread_mutex_t *mutex,
                  pthread_cond_t *cond);
void write_ctype(int *pipe_ready, int fdout, c_type old_type,
                 pthread_mutex_t *mutex, pthread_cond_t *cond);

void sync_read(int *pipe_ready, int fd, void * addr, size_t size,
               pthread_mutex_t *mutex, pthread_cond_t *cond) {
  pthread_mutex_lock(mutex);
  while (! *pipe_ready) {
     pthread_cond_wait(cond, mutex);
  }
  read(fd, addr, size);
  *pipe_ready = 0;
  pthread_cond_signal(cond);
  pthread_mutex_unlock(mutex);
}

void sync_write(int *pipe_ready, int fd, void * addr,size_t size,
                pthread_mutex_t *mutex, pthread_cond_t *cond) {
  pthread_mutex_lock(mutex);
  while (*pipe_ready) {
     pthread_cond_wait(cond, mutex);
  }
  write(fd, addr, size);
  *pipe_ready = 1;
  pthread_cond_signal(cond);
  pthread_mutex_unlock(mutex);
}

c_decl_array fork_ffi_parse(int argc, const char** argv) {
  pid_t pid;

  c_decl_array decls;
  int fd_data[2];
  unsigned int i;

  int * pipe_ready = NULL;
  pthread_cond_t * cond = NULL;
  pthread_condattr_t condattr;
  pthread_mutex_t * mutex = NULL;
  pthread_mutexattr_t mutexattr;

  mutex = (pthread_mutex_t *)
    mmap(NULL, sizeof(pthread_mutex_t), PROT_READ | PROT_WRITE,
         MAP_SHARED | MAP_ANONYMOUS, -1, 0);

  cond = (pthread_cond_t*)
    mmap(NULL, sizeof(pthread_cond_t), PROT_READ | PROT_WRITE,
         MAP_SHARED | MAP_ANONYMOUS, -1, 0);

  pipe_ready = (int*)
    mmap(NULL, sizeof(int), PROT_READ | PROT_WRITE,
         MAP_SHARED | MAP_ANONYMOUS, -1, 0);

  *pipe_ready = 0;

  pthread_condattr_init(&condattr);
  pthread_condattr_setpshared(&condattr, PTHREAD_PROCESS_SHARED);
  pthread_cond_init(cond, &condattr);

  pthread_mutexattr_init(&mutexattr);
  pthread_mutexattr_setpshared(&mutexattr, PTHREAD_PROCESS_SHARED);
  pthread_mutex_init(mutex, &mutexattr);

  pipe(fd_data);

  pid = fork();
  if (pid < 0) {
    fprintf(stderr, "Error: could not fork\n");
    exit(1);
  }
  else if (pid == 0) {
    close(fd_data[0]);

    c_decl_array decls_out = ffi_parse(argc, argv);
    sync_write(pipe_ready, fd_data[1], &(decls_out.length),
               sizeof(unsigned long), mutex, cond);

    for (i = 0; i < decls_out.length; ++i) {
      write_decl(pipe_ready, fd_data[1], decls_out.data[i], mutex, cond);
    }

    free_decl_array(decls_out);
    exit(0);
  }
  else {
    close(fd_data[1]);

    sync_read(pipe_ready, fd_data[0], &(decls.length),
      sizeof(unsigned long), mutex, cond);

    decls.data = (c_decl*) malloc(sizeof(c_decl) * decls.length);

    for (i = 0; i < decls.length; ++i) {
      decls.data[i] = read_decl(pipe_ready, fd_data[0], mutex, cond);
    }

    wait(NULL);
  }
  return decls;
}

c_decl read_decl(int * pipe_ready, int fdin, pthread_mutex_t *mutex, pthread_cond_t *cond) {
  c_decl new_decl;
  size_t read_size, tmp;

  /* (0) read c_decl_id */
  sync_read(pipe_ready, fdin, &(new_decl.base), sizeof(c_decl_id), mutex, cond);

  /* (1) read size of name */
  tmp = sizeof(size_t);
  sync_read(pipe_ready, fdin, &read_size, tmp, mutex, cond);

  /* (2) read name */
  new_decl.name = (char *) malloc(read_size + 1);
  sync_read(pipe_ready, fdin, new_decl.name, read_size, mutex, cond);
  new_decl.name[read_size] = '\0';

  /* (3) read size of type str */
  tmp = sizeof(size_t);
  sync_read(pipe_ready, fdin, &read_size, tmp, mutex, cond);

  /* (4) read type string */
  new_decl.type_str = (char*) malloc(read_size + 1);
  sync_read(pipe_ready, fdin, new_decl.type_str, read_size, mutex, cond);
  new_decl.type_str[read_size] = '\0';


  /* (5) read size of val */
  tmp = sizeof(size_t);
  sync_read(pipe_ready, fdin, &read_size, tmp, mutex, cond);
  new_decl.val_size = read_size;

  /* (6) read val */
  new_decl.val = malloc(read_size);
  sync_read(pipe_ready, fdin, new_decl.val, read_size, mutex, cond);

  /* read new_decl.ctype */
  new_decl.ctype = read_ctype(pipe_ready, fdin, mutex, cond);
  return new_decl;
}

c_type read_ctype(int *pipe_ready, int fdin, pthread_mutex_t *mutex, pthread_cond_t *cond) {
  c_type new_type;
  unsigned int i;
  /* (0) read ctype */
  sync_read(pipe_ready, fdin, &new_type, sizeof(c_type), mutex,cond);

  new_type.fields = (c_type*) malloc(sizeof(c_type) * new_type.field_length);

  for (i = 0; i < new_type.field_length; ++i) {
    new_type.fields[i] = read_ctype(pipe_ready, fdin, mutex, cond);
  }
  return new_type;
}

void write_decl(int *pipe_ready, int fdout, c_decl old_decl,
                pthread_mutex_t *mutex, pthread_cond_t *cond) {
  size_t write_size, tmp;

  /* (0) write c_decl_id */
  sync_write(pipe_ready, fdout, &(old_decl.base), sizeof(c_decl_id), mutex, cond);

  /* (1) write size of name */
  write_size = tmp = strlen(old_decl.name);
  sync_write(pipe_ready, fdout, &write_size, sizeof(size_t), mutex, cond);

  /* (2) write name */
  sync_write(pipe_ready, fdout, old_decl.name, write_size, mutex, cond);

  /* (3) write size of type str */
  write_size = tmp =strlen(old_decl.type_str);
  sync_write(pipe_ready, fdout, &write_size, sizeof(size_t), mutex, cond);

  /* (4) write type string */
  sync_write(pipe_ready, fdout, old_decl.type_str, write_size, mutex,cond);

  /* (5) write size of val */
  /* TODO c_decl should have a decl.size field
     this is a place holder until then */
  write_size = tmp = sizeof(uint64_t);
  sync_write(pipe_ready, fdout, &write_size, tmp, mutex,cond);

  /* (6) write val */
  if (old_decl.val == NULL) {
    uint64_t tmp = 0;
    sync_write(pipe_ready, fdout, &tmp, write_size, mutex, cond);

  }
  else {
    sync_write(pipe_ready, fdout, old_decl.val, write_size, mutex, cond);
  }

  /* write old_decl.ctype */
  write_ctype(pipe_ready, fdout, old_decl.ctype, mutex, cond);
}

void write_ctype(int *pipe_ready, int fdout, c_type old_type, pthread_mutex_t *mutex, pthread_cond_t *cond) {
  unsigned int i;

  /* (0) write ctype */
  sync_write(pipe_ready, fdout, &old_type, sizeof(c_type), mutex, cond);

  for (i = 0; i < old_type.field_length; ++i) {
    write_ctype(pipe_ready, fdout, old_type.fields[i], mutex, cond);
  }
}

#ifdef __cplusplus
}
#endif
