def foo(x:Int):Int=
{
  def bar(y:Int):Int=
  {
    def baz(z:Int):Int=
    {
      if(z<6){6}
      else {foo(z)}
    }
    baz(y)
  }
  baz(x)
}

println(foo(5))
