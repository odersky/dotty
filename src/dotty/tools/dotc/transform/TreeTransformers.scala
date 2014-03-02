package dotty.tools.dotc
package transform

import ast.Trees._
import core.Contexts._
import annotation.tailrec

object TreeTransformers {

  import ast.tpd._

  class TreeTransform {
    def prepareForIdent(tree: Ident): TreeTransform = this
    def prepareForSelect(tree: Select): TreeTransform = this
    def prepareForApply(tree: Apply): TreeTransform = this
    def prepareForBlock(tree: Block): TreeTransform = this
    def prepareForValDef(tree: ValDef): TreeTransform = this
    def prepareForStats(trees: List[Tree]): TreeTransform = this

    def transformIdent(tree: Ident)(implicit ctx: Context): Tree = tree
    def transformSelect(tree: Select)(implicit ctx: Context): Tree = tree
    def transformApply(tree: Apply)(implicit ctx: Context): Tree = tree
    def transformBlock(tree: Block)(implicit ctx: Context): Tree = tree
    def transformValDef(tree: ValDef)(implicit ctx: Context): Tree = tree
    def transformStats(stats: List[Tree])(implicit ctx: Context): List[Tree] = stats
  }

  private def mapTransforms[T](ts: List[TreeTransform], f: TransformerMap[T], x: T): List[TreeTransform] = ts match {
    case t :: ts1 =>
      val mt = f(t, x)
      val mts1 = mapTransforms(ts1, f, x)
      if ((mt eq t) && (mts1 eq ts1)) ts else mt :: mts1
    case nil =>
      nil
  }

  def transform(transforms: List[TreeTransform], tree: Tree)(implicit ctx: Context): Tree = {
    def mappedTransforms[T <: Tree](f: TransformerMap[T], tree: T): List[TreeTransform] =
      mapTransforms(transforms, f, tree)

    def go(ts: List[TreeTransform], last: Tree): Tree = last match {
      case last: Ident => goIdent(ts, last)
      case last: Select => goSelect(ts, last)
      case last: Apply => goApply(ts, last)
      case last: Block => goBlock(ts, last)
      case last: ValDef => goValDef(ts, last)
    }

    def goIdent(ts: List[TreeTransform], last: Ident): Tree = ts match {
      case t :: ts1 =>
        t.transformIdent(last) match {
          case next: Ident => goIdent(ts1, next)
          case next => go(ts, next)
        }
      case _ =>
        last
    }

    def goSelect(ts: List[TreeTransform], last: Select): Tree = ts match {
      case t :: ts1 =>
        t.transformSelect(last) match {
          case next: Select => goSelect(ts1, next)
          case next => go(ts, next)
        }
      case _ =>
        last
    }

    def goApply(ts: List[TreeTransform], last: Apply): Tree = ts match {
      case t :: ts1 =>
        t.transformApply(last) match {
          case next: Apply => goApply(ts1, next)
          case next => go(ts, next)
        }
      case _ =>
        last
    }

    def goBlock(ts: List[TreeTransform], last: Block): Tree = ts match {
      case t :: ts1 =>
        t.transformBlock(last) match {
          case next: Block => goBlock(ts1, next)
          case next => go(ts, next)
        }
      case _ =>
        last
    }

    def goValDef(ts: List[TreeTransform], last: ValDef): Tree = ts match {
      case t :: ts1 =>
        t.transformValDef(last)(ctx.fresh withOwner last.symbol) match {
          case next: ValDef => goValDef(ts1, next)
          case next => go(ts, next)
        }
      case nil =>
        last
    }

    tree match {
      case tree: Ident =>
        val ts1 = mappedTransforms(prepareForIdentFn, tree)
        goIdent(ts1, tree)
      case tree: Select =>
        val ts1 = mappedTransforms(prepareForSelectFn, tree)
        val tree1 = cpy.Select(tree, transform(ts1, tree.qualifier), tree.name)
        goSelect(ts1, tree1)
      case tree: Apply =>
        val ts1 = mappedTransforms(prepareForApplyFn, tree)
        val tree1 = cpy.Apply(tree,
            transform(ts1, tree.fun),
            transform(ts1, tree.args))
        goApply(ts1, tree1)
      case tree: Block =>
        val ts1 = mappedTransforms(prepareForBlockFn, tree)
        val tree1 = cpy.Block(tree,
            transformStats(ts1, tree.stats),
            transform(ts1, tree.expr))
        goBlock(ts1, tree1)
       case tree: ValDef =>
        val ts1 = mappedTransforms(prepareForValDefFn, tree)
        val nestedCtx = ctx.fresh withOwner tree.symbol
        val tree1 = cpy.ValDef(tree, tree.mods, tree.name,
            transform(ts1, tree.tpt),
            transform(ts1, tree.rhs)(nestedCtx))
        goValDef(ts1, tree1)
    }
  }

  def transform(transforms: List[TreeTransform], trees: List[Tree])(implicit ctx: Context): List[Tree] =
    trees mapConserve (transform(transforms, _))

  def transformStats(transforms: List[TreeTransform], trees: List[Tree])(implicit ctx: Context): List[Tree] = {
    def go(ts: List[TreeTransform], last: List[Tree]): List[Tree] = ts match {
      case t :: ts1 => go(ts1, t.transformStats(last))
      case nil => last
    }
    val ts1 = mapTransforms(transforms, prepareForStatsFn, trees)
    val trees1 = transform(ts1, trees)
    go(ts1, trees1)
  }

  private type TransformerMap[T] = (TreeTransform, T) => TreeTransform

  private val prepareForIdentFn: TransformerMap[Ident] = _ prepareForIdent _
  private val prepareForSelectFn: TransformerMap[Select] = _ prepareForSelect _
  private val prepareForApplyFn: TransformerMap[Apply] = _ prepareForApply _
  private val prepareForBlockFn: TransformerMap[Block] = _ prepareForBlock _
  private val prepareForValDefFn: TransformerMap[ValDef] = _ prepareForValDef _
  private val prepareForStatsFn: TransformerMap[List[Tree]] = _ prepareForStats _
}