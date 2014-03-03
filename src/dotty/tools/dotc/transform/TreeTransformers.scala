package dotty.tools.dotc
package transform

import ast.Trees._
import core.Contexts._
import annotation.tailrec

object TreeTransforms {
  import ast.tpd._

  /** The base class of tree transforms. For each kind of tree K, there are
   *  two methods which can be overridden:
   *
   *      prepareForK  // return a new TreeTransform which gets applied to the K
   *                   // node and its children
   *      transformK   // transform node of type K
   *
   *  If a transform does not need to visit a node or any of its children, it
   *  signals this fact by returning a NoTransform from a prepare method.
   *
   *  If all transforms in a group are NoTransforms, the tree is no longer traversed.
   */
  class TreeTransform {
    def prepareForIdent(tree: Ident): TreeTransform = this
    def prepareForSelect(tree: Select): TreeTransform = this
    def prepareForThis(tree: This): TreeTransform = this
    def prepareForSuper(tree: Super): TreeTransform = this
    def prepareForApply(tree: Apply): TreeTransform = this
    def prepareForTypeApply(tree: TypeApply): TreeTransform = this
    def prepareForLiteral(tree: Literal): TreeTransform = this
    def prepareForNew(tree: New): TreeTransform = this
    def prepareForPair(tree: Pair): TreeTransform = this
    def prepareForTyped(tree: Typed): TreeTransform = this
    def prepareForAssign(tree: Assign): TreeTransform = this
    def prepareForBlock(tree: Block): TreeTransform = this
    def prepareForIf(tree: If): TreeTransform = this
    def prepareForClosure(tree: Closure): TreeTransform = this
    def prepareForMatch(tree: Match): TreeTransform = this
    def prepareForCaseDef(tree: CaseDef): TreeTransform = this
    def prepareForReturn(tree: Return): TreeTransform = this
    def prepareForTry(tree: Try): TreeTransform = this
    def prepareForThrow(tree: Throw): TreeTransform = this
    def prepareForSeqLiteral(tree: SeqLiteral): TreeTransform = this
    def prepareForTypeTree(tree: TypeTree): TreeTransform = this
    def prepareForSelectFromTypeTree(tree: SelectFromTypeTree): TreeTransform = this
    def prepareForBind(tree: Bind): TreeTransform = this
    def prepareForAlternative(tree: Alternative): TreeTransform = this
    def prepareForUnApply(tree: UnApply): TreeTransform = this
    def prepareForValDef(tree: ValDef): TreeTransform = this
    def prepareForDefDef(tree: DefDef): TreeTransform = this
    def prepareForTypeDef(tree: TypeDef): TreeTransform = this
    def prepareForTemplate(tree: Template): TreeTransform = this
    def prepareForPackageDef(tree: PackageDef): TreeTransform = this
    def prepareForStats(trees: List[Tree]): TreeTransform = this

    def transformIdent(tree: Ident)(implicit ctx: Context): Tree = tree
    def transformSelect(tree: Select)(implicit ctx: Context): Tree = tree
    def transformThis(tree: This)(implicit ctx: Context): Tree = tree
    def transformSuper(tree: Super)(implicit ctx: Context): Tree = tree
    def transformApply(tree: Apply)(implicit ctx: Context): Tree = tree
    def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree = tree
    def transformLiteral(tree: Literal)(implicit ctx: Context): Tree = tree
    def transformNew(tree: New)(implicit ctx: Context): Tree = tree
    def transformPair(tree: Pair)(implicit ctx: Context): Tree = tree
    def transformTyped(tree: Typed)(implicit ctx: Context): Tree = tree
    def transformAssign(tree: Assign)(implicit ctx: Context): Tree = tree
    def transformBlock(tree: Block)(implicit ctx: Context): Tree = tree
    def transformIf(tree: If)(implicit ctx: Context): Tree = tree
    def transformClosure(tree: Closure)(implicit ctx: Context): Tree = tree
    def transformMatch(tree: Match)(implicit ctx: Context): Tree = tree
    def transformCaseDef(tree: CaseDef)(implicit ctx: Context): Tree = tree
    def transformReturn(tree: Return)(implicit ctx: Context): Tree = tree
    def transformTry(tree: Try)(implicit ctx: Context): Tree = tree
    def transformThrow(tree: Throw)(implicit ctx: Context): Tree = tree
    def transformSeqLiteral(tree: SeqLiteral)(implicit ctx: Context): Tree = tree
    def transformTypeTree(tree: TypeTree)(implicit ctx: Context): Tree = tree
    def transformSelectFromTypeTree(tree: SelectFromTypeTree)(implicit ctx: Context): Tree = tree
    def transformBind(tree: Bind)(implicit ctx: Context): Tree = tree
    def transformAlternative(tree: Alternative)(implicit ctx: Context): Tree = tree
    def transformUnApply(tree: UnApply)(implicit ctx: Context): Tree = tree
    def transformValDef(tree: ValDef)(implicit ctx: Context): Tree = tree
    def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = tree
    def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = tree
    def transformTemplate(tree: Template)(implicit ctx: Context): Tree = tree
    def transformPackageDef(tree: PackageDef)(implicit ctx: Context): Tree = tree
    def transformStats(stats: List[Tree])(implicit ctx: Context): List[Tree] = stats
  }

  val NoTransform = new TreeTransform

  /** A map that takes a TreeTransformer and some other element and returns a TreeTransformer */
  private type TransformerMap[T] = (TreeTransform, T) => TreeTransform

  private val prepareForIdentFn: TransformerMap[Ident] = _ prepareForIdent _
  private val prepareForSelectFn: TransformerMap[Select] = _ prepareForSelect _
  private val prepareForApplyFn: TransformerMap[Apply] = _ prepareForApply _
  private val prepareForBlockFn: TransformerMap[Block] = _ prepareForBlock _
  private val prepareForValDefFn: TransformerMap[ValDef] = _ prepareForValDef _
  private val prepareForStatsFn: TransformerMap[List[Tree]] = _ prepareForStats _

  /** Apply a transformer map to an array of tree transformers.
   *  @param  f  the TransformerMap to apply
   *  @param  nx An index array which yields for each index i the index of the next
   *          higher transform t such that f(t, x) is not the empty default defined in TreeTransform.
   *  @param  x  The element to pass as second parameter to the transformer map f.
   *  @param  ts The array of tree transforms to map.
   */
  private def mapTransforms[T](f: TransformerMap[T], nx: Array[Int], x: T, ts: Array[TreeTransform]): Array[TreeTransform] = {
    var allDone = true
    var result = ts
    var i = nx(0)
    while (i < ts.length) {
      val t = ts(i)
      allDone = allDone && (t eq NoTransform)
      val mt = f(t, x)
      if (mt ne t) {
        if (result eq ts) result = ts.clone
        result(i) = mt
      }
      i = nx(i + 1)
    }
    if (allDone) null else result
  }
}

import TreeTransforms._

/** A group of tree transforms that are applied in sequence during the same phase */
class TreeTransforms(transforms: Array[TreeTransform]) {
  def this(ts: TreeTransform*) = this(ts.toArray)

  import ast.tpd._

  /** Transform `tree` using the given tree transforms */
  def transform(tree: Tree)(implicit ctx: Context): Tree = transform(transforms, tree)

  /** Transform `tree` using the given tree transforms */
  def transformStats(stats: List[Tree])(implicit ctx: Context): List[Tree] = transformStats(transforms, stats)

  private def hasMethod(cls: Class[_], name: String): Boolean =
    if (cls.getDeclaredMethods.exists(_.getName == name)) cls != classOf[TreeTransform]
    else hasMethod(cls.getSuperclass, name)

  /** Create an index array `next` of size one larger than teh size of `transforms` such that
   *  for each index i, `next(i)` is the smallest index j such that
   *
   *      i <= j
   *      j == transforms.length || transform(j) defines a non-default method with given `name`
   */
  private def index(name: String): Array[Int] = {
    val len = transforms.length
    var i = 0
    val next = new Array[Int](len + 1)
    while (i <= len) {
      var j = i
      while (j < len && !hasMethod(transforms(j).getClass, name))
        j += 1
      while (i <= j) {
        next(i) = j
        i += 1
      }
    }
    next
  }

  val nxPrepIdent = index("prepareForIdent")
  val nxPrepSelect = index("prepareForSelect")
  val nxPrepApply = index("prepareForApply")
  val nxPrepBlock = index("prepareForBlock")
  val nxPrepValDef = index("prepareForValDef")
  val nxPrepStats = index("prepareForStats")

  val nxTransIdent = index("transformIdent")
  val nxTransSelect = index("transformSelect")
  val nxTransApply = index("transformApply")
  val nxTransBlock = index("transformBlock")
  val nxTransValDef = index("transformValDef")
  val nxTransStats = index("transformStats")
  // ...

  /** Transform given tree using given transforms */
  private def transform(transforms: Array[TreeTransform], tree: Tree)(implicit ctx: Context): Tree = {
    def mappedTransforms[T <: Tree](f: TransformerMap[T], nx: Array[Int], tree: T): Array[TreeTransform] =
      mapTransforms(f, nx, tree, transforms)

    /** Fallback dispatch if we do not know the type of the tree to transform
     *  @param  ts   the array of transforms to apply
     *  @param  i    the idnex of the next transform to apply
     *  @param  last the tree to apply the transform to
     */
    def go(ts: Array[TreeTransform], i: Int, last: Tree): Tree = last match {
      case last: Ident => goIdent(ts, i, last)
      case last: Select => goSelect(ts, i, last)
      case last: Apply => goApply(ts, i, last)
      case last: Block => goBlock(ts, i, last)
      case last: ValDef => goValDef(ts, i, last)
    }

    /** Specialized dispatch for idents. Optimized for the case where
     *  the next transform returns again an Ident
     */
    def goIdent(ts: Array[TreeTransform], i: Int, last: Ident): Tree =
      if (i == ts.length) last
      else ts(i).transformIdent(last) match {
        case next: Ident => goIdent(ts, nxTransIdent(i + 1), next)
        case next => go(ts, nxTransIdent(i + 1), next)
      }

    def goSelect(ts: Array[TreeTransform], i: Int, last: Select): Tree =
      if (i == ts.length) last
      else ts(i).transformSelect(last) match {
        case next: Select => goSelect(ts, nxTransSelect(i + 1), next)
        case next => go(ts, nxTransSelect(i + 1), next)
      }

    def goApply(ts: Array[TreeTransform], i: Int, last: Apply): Tree =
      if (i == ts.length) last
      else ts(i).transformApply(last) match {
        case next: Apply => goApply(ts, nxTransApply(i + 1), next)
        case next => go(ts, nxTransApply(i + 1), next)
      }

    def goBlock(ts: Array[TreeTransform], i: Int, last: Block): Tree =
      if (i == ts.length) last
      else ts(i).transformBlock(last) match {
        case next: Block => goBlock(ts, nxTransBlock(i + 1), next)
        case next => go(ts, nxTransBlock(i + 1), next)
      }

    def goValDef(ts: Array[TreeTransform], i: Int, last: ValDef): Tree =
      if (i == ts.length) last
      else ts(i).transformValDef(last) match {
        case next: ValDef => goValDef(ts, nxTransValDef(i + 1), next)
        case next => go(ts, nxTransValDef(i + 1), next)
      }

    /** Main pattern match. For each node, perform the following steps
     *  (1) Compute the array of new transforms for this node and its children.
     *      If the result is null, return tree unchanged
     *  (2) Transforms recursively to all children and reconstitute the same kind of node
     *      with the transformed children.
     *  (3) Apply all transforms in sequence to the node resulting from (2).
     *
     *  The algorithm takes care using indices not to execute transforms which
     *  are empty for the node.
     */
    tree match {
      case tree: Ident =>
        val ts1 = mappedTransforms(prepareForIdentFn, nxPrepIdent, tree)
        if (ts1 == null) tree
        else goIdent(ts1, nxTransIdent(0), tree)
      case tree: Select =>
        val ts1 = mappedTransforms(prepareForSelectFn, nxPrepSelect, tree)
        if (ts1 == null) tree
        else {
          val tree1 = cpy.Select(tree, transform(ts1, tree.qualifier), tree.name)
          goSelect(ts1, nxTransSelect(0), tree1)
        }
      case tree: Apply =>
        val ts1 = mappedTransforms(prepareForApplyFn, nxPrepApply, tree)
        if (ts1 == null) tree
        else {
          val tree1 = cpy.Apply(tree,
            transform(ts1, tree.fun),
            transform(ts1, tree.args))
          goApply(ts1, nxTransApply(0), tree1)
        }
      case tree: Block =>
        val ts1 = mappedTransforms(prepareForBlockFn, nxPrepBlock, tree)
        if (ts1 == null) tree
        else {
          val tree1 = cpy.Block(tree,
              transformStats(ts1, tree.stats),
              transform(ts1, tree.expr))
          goBlock(ts1, nxTransBlock(0), tree1)
        }
      case EmptyValDef => tree
      case tree: ValDef =>
        val ts1 = mappedTransforms(prepareForValDefFn, nxPrepValDef, tree)
        if (ts1 == null) tree
        else {
          val nestedCtx = ctx.fresh withOwner tree.symbol
          val tree1 = cpy.ValDef(tree, tree.mods, tree.name,
            transform(ts1, tree.tpt),
            transform(ts1, tree.rhs)(nestedCtx))
          goValDef(ts1, nxTransValDef(0), tree1)
        }
    }
  }

  /** Transform list of trees, flatten if necessary */
  private def transform(transforms: Array[TreeTransform], trees: List[Tree])(implicit ctx: Context): List[Tree] =
    flatten(trees mapConserve (transform(transforms, _)))

  /** Transform list of statements using the transforms' `transformStats` method, in the same
   *  way individual tree nodes are transformed.
   */
  private def transformStats(transforms: Array[TreeTransform], trees: List[Tree])(implicit ctx: Context): List[Tree] = {
    def go(ts: Array[TreeTransform], i: Int, last: List[Tree]): List[Tree] =
      if (i == ts.length) last
      else go(ts, nxTransStats(i + 1), ts(i).transformStats(last))
    val ts1 = mapTransforms(prepareForStatsFn, nxPrepStats, trees, transforms)
    if (ts1 == null) trees
    else {
      val trees1 = transform(ts1, trees)
      go(ts1, nxTransStats(0), trees1)
    }
  }
}