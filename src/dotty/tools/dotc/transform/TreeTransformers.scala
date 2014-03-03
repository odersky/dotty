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

  private def mapTransforms[T](ts: Array[TreeTransform], f: TransformerMap[T], x: T): Array[TreeTransform] = {
    var result = ts
    var i = 0
    while (i < ts.length) {
      val t = ts(i)
      val mt = f(t, x)
      if (mt ne t) {
        if (result eq ts) result = ts.clone
        result(i) = mt
      }
      i += 1
    }
    result
  }

  def transform(transforms: Array[TreeTransform], tree: Tree)(implicit ctx: Context): Tree = {
    def mappedTransforms[T <: Tree](f: TransformerMap[T], tree: T): Array[TreeTransform] =
      mapTransforms(transforms, f, tree)

    def go(ts: Array[TreeTransform], i: Int, last: Tree): Tree = last match {
      case last: Ident => goIdent(ts, i, last)
      case last: Select => goSelect(ts, i, last)
      case last: Apply => goApply(ts, i, last)
      case last: Block => goBlock(ts, i, last)
      case last: ValDef => goValDef(ts, i, last)
    }

    def goIdent(ts: Array[TreeTransform], i: Int, last: Ident): Tree =
      if (i == ts.length) last
      else ts(i).transformIdent(last) match {
        case next: Ident => goIdent(ts, i + 1, next)
        case next => go(ts, i + 1, next)
      }

    def goSelect(ts: Array[TreeTransform], i: Int, last: Select): Tree =
      if (i == ts.length) last
      else ts(i).transformSelect(last) match {
        case next: Select => goSelect(ts, i + 1, next)
        case next => go(ts, i + 1, next)
      }

    def goApply(ts: Array[TreeTransform], i: Int, last: Apply): Tree =
      if (i == ts.length) last
      else ts(i).transformApply(last) match {
        case next: Apply => goApply(ts, i + 1, next)
        case next => go(ts, i + 1, next)
      }

    def goBlock(ts: Array[TreeTransform], i: Int, last: Block): Tree =
      if (i == ts.length) last
      else ts(i).transformBlock(last) match {
        case next: Block => goBlock(ts, i + 1, next)
        case next => go(ts, i + 1, next)
      }

    def goValDef(ts: Array[TreeTransform], i: Int, last: ValDef): Tree =
      if (i == ts.length) last
      else ts(i).transformValDef(last) match {
        case next: ValDef => goValDef(ts, i + 1, next)
        case next => go(ts, i + 1, next)
      }

    tree match {
      case tree: Ident =>
        val ts1 = mappedTransforms(prepareForIdentFn, tree)
        goIdent(ts1, 0, tree)
      case tree: Select =>
        val ts1 = mappedTransforms(prepareForSelectFn, tree)
        val tree1 = cpy.Select(tree, transform(ts1, tree.qualifier), tree.name)
        goSelect(ts1, 0, tree1)
      case tree: Apply =>
        val ts1 = mappedTransforms(prepareForApplyFn, tree)
        val tree1 = cpy.Apply(tree,
            transform(ts1, tree.fun),
            transform(ts1, tree.args))
        goApply(ts1, 0, tree1)
      case tree: Block =>
        val ts1 = mappedTransforms(prepareForBlockFn, tree)
        val tree1 = cpy.Block(tree,
            transformStats(ts1, tree.stats),
            transform(ts1, tree.expr))
        goBlock(ts1, 0, tree1)
       case tree: ValDef =>
        val ts1 = mappedTransforms(prepareForValDefFn, tree)
        val nestedCtx = ctx.fresh withOwner tree.symbol
        val tree1 = cpy.ValDef(tree, tree.mods, tree.name,
            transform(ts1, tree.tpt),
            transform(ts1, tree.rhs)(nestedCtx))
        goValDef(ts1, 0, tree1)
    }
  }

  def transform(transforms: Array[TreeTransform], trees: List[Tree])(implicit ctx: Context): List[Tree] =
    trees mapConserve (transform(transforms, _))

  def transformStats(transforms: Array[TreeTransform], trees: List[Tree])(implicit ctx: Context): List[Tree] = {
    def go(ts: Array[TreeTransform], i: Int, last: List[Tree]): List[Tree] =
      if (i == ts.length) last
      else go(ts, i + 1, ts(i).transformStats(last))
    val ts1 = mapTransforms(transforms, prepareForStatsFn, trees)
    val trees1 = transform(ts1, trees)
    go(ts1, 0, trees1)
  }

  private type TransformerMap[T] = (TreeTransform, T) => TreeTransform

  private val prepareForIdentFn: TransformerMap[Ident] = _ prepareForIdent _
  private val prepareForSelectFn: TransformerMap[Select] = _ prepareForSelect _
  private val prepareForApplyFn: TransformerMap[Apply] = _ prepareForApply _
  private val prepareForBlockFn: TransformerMap[Block] = _ prepareForBlock _
  private val prepareForValDefFn: TransformerMap[ValDef] = _ prepareForValDef _
  private val prepareForStatsFn: TransformerMap[List[Tree]] = _ prepareForStats _
}