#include <ciao_gluecode.h>

void create_lock(char *);
bool_t gluecode_create_lock(worker_t *w) {
  ERR__FUNCTOR("file_lock:create_lock", 1);
  ciao_term t0;
  char *c0;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  if (!ciao_is_atom_s(state, t0)) ERROR_IN_ARG(X(0), 0 + 1, STRICT_ATOM);
  c0 = ciao_atom_name_dup_s(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(create_lock(c0));
  free(c0);
  ciao_frame_end_s(state);
  return TRUE;
}

void file_lock(char *);
bool_t gluecode_file_lock(worker_t *w) {
  ERR__FUNCTOR("file_lock:file_lock", 1);
  ciao_term t0;
  char *c0;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  if (!ciao_is_atom_s(state, t0)) ERROR_IN_ARG(X(0), 0 + 1, STRICT_ATOM);
  c0 = ciao_atom_name_dup_s(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(file_lock(c0));
  free(c0);
  ciao_frame_end_s(state);
  return TRUE;
}

void file_unlock(char *);
bool_t gluecode_file_unlock(worker_t *w) {
  ERR__FUNCTOR("file_lock:file_unlock", 1);
  ciao_term t0;
  char *c0;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  if (!ciao_is_atom_s(state, t0)) ERROR_IN_ARG(X(0), 0 + 1, STRICT_ATOM);
  c0 = ciao_atom_name_dup_s(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(file_unlock(c0));
  free(c0);
  ciao_frame_end_s(state);
  return TRUE;
}

void clear_lock(char *);
bool_t gluecode_clear_lock(worker_t *w) {
  ERR__FUNCTOR("file_lock:clear_lock", 1);
  ciao_term t0;
  char *c0;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  if (!ciao_is_atom_s(state, t0)) ERROR_IN_ARG(X(0), 0 + 1, STRICT_ATOM);
  c0 = ciao_atom_name_dup_s(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(clear_lock(c0));
  free(c0);
  ciao_frame_end_s(state);
  return TRUE;
}

void file_lock_init(char *module) {
  define_c_mod_predicate(module, "create_lock", 1, gluecode_create_lock);
  define_c_mod_predicate(module, "file_lock", 1, gluecode_file_lock);
  define_c_mod_predicate(module, "file_unlock", 1, gluecode_file_unlock);
  define_c_mod_predicate(module, "clear_lock", 1, gluecode_clear_lock);
}

void file_lock_end(char *module) {
  undefine_c_mod_predicate(module, "create_lock", 1);
  undefine_c_mod_predicate(module, "file_lock", 1);
  undefine_c_mod_predicate(module, "file_unlock", 1);
  undefine_c_mod_predicate(module, "clear_lock", 1);
}

