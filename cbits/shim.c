#include "shim.h"

// May be useful:
/* rb_block_arity(void) */
/* VALUE rb_block_call(VALUE recv, ID mid, int argc, VALUE argv[], VALUE (*func) (ANYARGS), VALUE data2) */
/* rb_block_lambda(void) */
/* rb_block_proc(void) */
/* VALUE rb_define_finalizer(VALUE, VALUE); */
/* VALUE rb_undefine_finalizer(VALUE); */

FunPtrFn freeFunPtrFn = NULL;

VALUE rupee_new_float(double d) {
#ifdef RUBY2
  return rb_float_new_in_heap(d);
#else
  return rb_float_new(d);
#endif
}

void append_pinning_array(obj, array) {
  if (RTEST(array)) { 
    rb_ary_push(array, result);
  }
}

VALUE rupee_rb_get_singleton(VALUE obj, VALUE pin_array) {
  VALUE result = rb_singleton_class(obj);
  append_pinning_array(obj, pin_array);
  return result;
}

char rupee_rb_type(VALUE obj) {
  return rb_type(obj);
}

VALUE rupee_int2num(long x) {
  return INT2NUM(x);
}

long rupee_num2long(VALUE v) {
  return NUM2LONG(v);
}

double rupee_num2dbl(VALUE v) {
  NUM2DBL(v);
}

unsigned long rupee_rb_ary_len(VALUE x) {
  return RARRAY_LEN(x);
}

// not necessarily null terminated; clients must check len
char * rupee_rb2cstr(VALUE v) {
  return StringValuePtr(v);
}

VALUE rupee_rb_str_len(VALUE str) {
  return RSTRING_LEN(str);
}

void rupee_register_funptr_free(HsFunPtr ptr) {
  // reference to Haskell's hs_free_fun_ptr
  freeFunPtrFn = ptr;
}

void print_object(VALUE obj) {
  obj = rb_funcall2(obj, rb_intern("to_s"), 0, NULL);
  rb_funcall2(rb_mKernel, rb_intern("puts"), 1, &obj);
}

VALUE rupee_rb_funcall2_protected(struct s_dispatch* dispatch) {
  VALUE result;
  if (RTEST(dispatch->blk)) {
    result = rb_funcall_with_block(
        dispatch->self,
        dispatch->mid,
        dispatch->argc,
        dispatch->argv,
        dispatch->blk);
  } else {
    result = rb_funcall2(
        dispatch->self,
        dispatch->mid,
        dispatch->argc,
        dispatch->argv);
  }

  return result;
}

// this allows us to safely call Ruby methods.
// we use rb_protect to prevent problems around stack unwinding in the presense
// of throw/raise.
VALUE rupee_rb_funcall2(struct s_dispatch* dispatch, int* state, VALUE pinning_ary) {
  VALUE result = rb_protect(&rupee_rb_funcall2_protected, dispatch, state);
  if (RTEST(pinning_ary)) { 
    rb_ary_push(pinning_ary, result);
  }
  return result;
}

// all ruby methods will use this as their definition.
// this indirection allows us to raise (or re-raise) Ruby exceptions here,
// which is something we couldn't safely do from Haskell.
// (Ruby's exception mechanism uses setjmp/lngjmp)
VALUE rupee_haskell_method_invoke(int argc, VALUE *argv, VALUE obj) {
  hs_method fun = (hs_method)rupee_method_data();
  VALUE exception = Qnil;
  VALUE pin_ary = rb_ary_new();
  VALUE proc = Qnil;

  if(rb_block_given_p()) {
    proc = rb_block_proc();
  }

  VALUE result = fun(argc, argv, obj, &exception, pin_ary, proc);

  if (RTEST(exception)) {
    // TODO: handle strings vs objects vs exceptions
    /* puts("--> exception: "); */
    /* print_object(exception); */
    rb_exc_raise(exception);
  }

  return result;
}

// creates an object that, when GC'd, will free the attached funptr.
VALUE create_funptr_finalizer(HsFunPtr ptr) {
  return Data_Wrap_Struct(rb_cObject, NULL, freeFunPtrFn, ptr);
}

// a wrapper around rb_define_method
void rupee_define_method(VALUE klass, const char *name, VALUE (*func)(), int argc) {
  ID id = rb_intern(name);
  VALUE store;

  // keep the Haskell function in a table attached to the target class
  store = rb_attr_get(klass, rb_intern("__rupee__"));
  if (store == Qnil) {
    store = rb_obj_alloc(rb_cObject);
    rb_ivar_set(klass, rb_intern("__rupee__"), store);
  }
  rb_ivar_set(store, id, func);

  // attach a finalizer; if the class is GC'd, we should free the FunPtr
  store = rb_attr_get(klass, rb_intern("__rupee__gc"));
  if (store == Qnil) {
    store = rb_obj_alloc(rb_cObject);
    rb_ivar_set(klass, rb_intern("__rupee__gc"), store);
  }
  rb_ivar_set(store, id, create_funptr_finalizer(func));

  // finally, define the method, using our wrapper
  rb_define_method(klass, name, rupee_haskell_method_invoke, -1);
}

VALUE rupee_method_data() {
  ID id;
  VALUE klass;
  if (!rb_frame_method_id_and_class(&id, &klass)) {
    rb_raise(
        rb_eRuntimeError,
        "Cannot get method id and class for function");
  }

  if (rb_type(klass) == T_ICLASS) {
    klass = rb_class_of(klass);
  }

  VALUE store = rb_ivar_get(klass, rb_intern("__rupee__"));
  return (store == Qnil) ? Qnil : rb_ivar_get(store, id);
}
