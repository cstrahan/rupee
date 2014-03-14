CONS = Proc.new do |callable|
  # callable = callable.method("call").to_proc
  Proc.new {|*args,&blk|
    # callable.call(self, *args, &blk)
    # self.instance_exec(
      # self,
      # nil,
      # *args,
      # &callable)
    su = Proc.new {|*args,&blk| super(*args,&blk) }
    callable.call(
      self, 
      su,
      *args,
      &blk)
  }
end

class Foo
  def test
    puts "foo"
  end
end

class Bar < Foo
  callable = Object.new
  def callable.call(zelf, su, *args, &blk)
    su.call
    puts "bar"
  end

  # define_method "test" do
    # super()
    # puts "bar"
  # end
  define_method "test", &(CONS.call(callable))
end

Bar.new.test(nil,nil)

# puts fib.to_a.inspect

callable = Object.new
def callable.call(zelf, su, *args, &blk)
  puts zelf.inspect
end

p = CONS.call(callable)
"some string".instance_exec(nil,nil,&p)
# p.call(nil, nil)
# px = Proc.new { }
# p = Proc.new { px.call }
# (0..10_000_000).each do
  # Proc.new {}
  # callable.call(nil, nil)
  # p.call
  # "foo" + "bar" + "baz"
# end
