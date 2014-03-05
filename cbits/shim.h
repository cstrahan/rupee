#include "ffi.h"
#include <ruby.h>
#ifdef RUBY21
#include <ruby/intern.h>
#endif

typedef VALUE (*hs_method)(
    int    argc,
    VALUE* argv,
    VALUE  self,
    VALUE* exception,
    VALUE  pin_ary,
    VALUE  blk
    );

struct s_dispatch {
  VALUE  self;
  ID     mid;
  int    argc;
  VALUE* argv;
  VALUE  blk;
};

typedef void (*FunPtrFn)(HsFunPtr fn);

long arrayLength(VALUE r);
VALUE newFloat(double d);
int rubyType(VALUE obj);
VALUE int2num(long x);
long num2long(VALUE v);
double num2dbl(VALUE v);
VALUE rupee_rb_funcall2_protected(struct s_dispatch* dispatch);
VALUE rupee_rb_funcall2(struct s_dispatch* dispatch, int* state, VALUE pinning_ary);
VALUE rupee_haskell_method_invoke(int argc, VALUE *argv, VALUE obj);
void rupee_define_method(VALUE klass, const char *name, VALUE (*func)(), int argc);
VALUE rupee_method_data();
VALUE rupee_rb_lambda(struct s_dispatch* dispatch, int* state, VALUE pinning_ary);
VALUE rupee_rb_proc(struct s_dispatch* dispatch, int* state, VALUE pinning_ary);
