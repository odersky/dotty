package dotty.tools.dotc
package transform

import core._
import ast.Trees._
import TreeTransforms._
import Contexts._
import Flags._
import Names._, NameOps._
import collection.mutable

/** A trait that's assumed by the transformers that run right after typer.
 *  Ensures that trees are normalized when seen by other transforms. This means:
 *  (1) All module class definitions appear after their companion class definitions
 *  (2) There are no import clauses or named arguments
 *  (3) All trees designating types are instances of TypeTree
 */
trait PostTyperTransforms extends TreeTransforms {

  import ast.tpd._

  /** Reorder statements so that module classes always come after their companion classes */
  override def transformStats(stats: List[Tree])(implicit ctx: Context) = {
    val moduleClassDefs = mutable.Map[Name, Tree]()
    def reorder(stats: List[Tree]): List[Tree] = stats match {
      case (stat: TypeDef) :: stats1 if stat.symbol.isClass =>
        if (stat.symbol is Module) {
          moduleClassDefs += (stat.name -> stat)
          val stats1r = reorder(stats1)
          if (moduleClassDefs contains stat.name) stat :: stats1r else stats1r
        }
        else {
          val mclsName = stat.name.moduleClassName
          moduleClassDefs remove mclsName match {
            case Some(mcdef) => stat :: mcdef :: reorder(stats1)
            case None => stat :: reorder(stats1)
          }
        }
      case stat :: stats1 => stat :: reorder(stats1)
      case Nil => Nil
    }
    super.transformStats(reorder(stats))
  }

  /** Eliminate import statements; map all trees designating types to TypeTree instances */
  override def transform(tree: Tree)(implicit ctx: Context): Tree  = tree match {
    case tree: Import => EmptyTree
    case NamedArg(_, arg) => super.transform(arg)
    case tree: TypeTree => super.transform(tree)
    case tree => super.transform(if (tree.isType) TypeTree(tree.tpe) else tree)
  }
}