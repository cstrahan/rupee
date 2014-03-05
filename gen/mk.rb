lines = File.readlines(File.expand_path("../vars.txt", __FILE__))
vars = lines.map do |line|
  line[0..-3]
end

classes = vars.select {|v| v.start_with?("rb_c")}
modules = vars.select {|v| v.start_with?("rb_m")}
errors  = vars.select {|v| v.start_with?("rb_e")}

def clean_name(var)
  fun_name = "rb" + var[4..-1]
end

def gen_wrappers(vars)
  vars.each do |var|
    fun_name = clean_name var
    puts "#{fun_name} :: RBIO RValue"
    puts "#{fun_name} = RBIO $ lift $ peek <$> #{var}"
    puts ""
  end
end

def gen_foreign_decls(vars)
  max_len = vars.map(&:size).max
  vars.each do |var|
    if var.size == 0
      puts ""
      next
    end

    var_str = "\"&#{var}\""
    puts "foreign import ccall #{var_str.ljust(max_len+3)} #{var.ljust(max_len)} :: Ptr RValue"
  end
end

# ------------------------------------------------------------------------------

puts "{-# LANGUAGE ForeignFunctionInterface #-}"
puts ""
puts "module Foreign.Rupee.Builtins"
puts "  ("
puts "  -- * Classes"
classes.map {|x| clean_name x}.each {|x| puts "  , #{x}"}
puts "  -- * Modules"
modules.map {|x| clean_name x}.each {|x| puts "  , #{x}"}
puts "  -- * Exceptions"
errors.map {|x| clean_name x}.each {|x| puts "  , #{x}"}
puts "  -- * stdin, stdout, stderr"
puts "  , rbstdin"
puts "  , rbstdout"
puts "  , rbstderr"
puts "  ) where"
puts ""
puts "import Foreign.Rupee.CAPI"
puts "import Foreign.Ptr"
puts "import Foreign.Storable"
puts "import Control.Applicative"
puts "import Control.Monad"
puts "import Control.Monad.Trans"
puts ""
puts ""
puts "-- Classes"
gen_wrappers classes
puts ""
puts "-- Modules"
gen_wrappers modules
puts ""
puts "-- Exceptions"
gen_wrappers errors
puts ""
puts "-- stdin, stdout, stderr"
puts <<-CODE
rbstdin :: RBIO RValue
rbstdin = RBIO $ lift $ peek <$> rb_stdin

rbstdout :: RBIO RValue
rbstdout = RBIO $ lift $ peek <$> rb_stdin

rbstderr :: RBIO RValue
rbstderr = RBIO $ lift $ peek <$> rb_stdin
CODE
puts""
puts""
puts "-- Foreign decls"
gen_foreign_decls vars
