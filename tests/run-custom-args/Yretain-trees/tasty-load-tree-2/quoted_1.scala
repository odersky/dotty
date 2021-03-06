import scala.quoted._

import scala.tasty._

object Foo {

  inline def inspectBody(i: => Int): String =
    ${ inspectBodyImpl('i) }

  def inspectBodyImpl(x: Expr[Int])(implicit reflect: Reflection): Expr[String] = {
    import reflect._

    def definitionString(tree: Tree): Expr[String] = tree.symbol match {
      case IsClassDefSymbol(sym) => sym.tree.show.toExpr
      case IsDefDefSymbol(sym) => sym.tree.show.toExpr
      case IsValDefSymbol(sym) => sym.tree.show.toExpr
      case _ => '{"NO DEFINTION"}
    }

    x.unseal match {
      case Inlined(None, Nil, arg) => definitionString(arg)
      case arg => definitionString(arg) // TODO should all by name parameters be in an inline node
    }
  }
}
