import language.noAutoTupling // try with on and off

class A {
  def foo(a: Int) = 0
}

class RichA {
  def foo(a: String) = 0
  def foo(a: String, b: String) = 0
  def foo() = 0
}

object Test {

  implicit def AToRichA(a: A): RichA = new RichA

  val a = new A
  a.foo()
  a.foo(1)

  a.foo("")
}

// t0851 is essentially the same:
object test1 {
  case class Foo[T,T2](f : (T,T2) => String) extends (((T,T2)) => String){
    def apply(t : T) = (s:T2) => f(t,s)
    def apply(p : (T,T2)) = f(p._1,p._2)
  }
  implicit def g[T](f : (T,String) => String): test1.Foo[T,String] = Foo(f)
  def main(args : Array[String]) : Unit = {
    val f = (x:Int,s:String) => s + x
    println(f(1))
    ()
  }
}
object Main {
  def main(args : Array[String]): Unit = {
    val fn = (a : Int, str : String) => "a: " + a + ", str: " + str
    implicit def fx[T](f : (T,String) => String): T => String = (x:T) => f(x,null)
    println(fn(1))
    ()
  }
}
