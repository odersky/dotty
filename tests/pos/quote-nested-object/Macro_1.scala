
import scala.quoted._

object Macro {


  object Implementation {

    transparent def plus(transparent  n: Int, m: Int): Int = ~plus(n, '(m))

    def plus(n: Int, m: Expr[Int]): Expr[Int] =
      if (n == 0) m
      else '{ ~n.toExpr + ~m }

    object Implementation2 {

      transparent def plus(transparent  n: Int, m: Int): Int = ~plus(n, '(m))

      def plus(n: Int, m: Expr[Int]): Expr[Int] =
        if (n == 0) m
        else '{ ~n.toExpr + ~m }
    }
  }

}
