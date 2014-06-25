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

  a.foo("a", "b") // Dotty change in auto-tupling. Will not consider an implicit conversion on the qualifier
                  // to make auto-tupling work.
}
