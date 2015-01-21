package arrxml.arrow

import annotation.implicitNotFound
import arrxml.tree.Tree

@implicitNotFound(msg = "No instance in scope for ArrowTree[${=>>}].")
trait ArrowTree[=>>[-_, +_]] extends ArrowPlus[=>>] with ArrowIf[=>>] {
   /**
    * construct a leaf
    */
   def mkLeaf[T[_] : Tree, A, B](a : A) : (B =>> T[A]) =
      constA(implicitly[Tree[T]].mkLeaf(a))

   /**
    * construct an inner node
    */
   def mkTree[T[_] : Tree, A, B](n : A, children : List[T[A]]) : (B =>> T[A]) =
      constA(implicitly[Tree[T]].mkTree[A, B](n, children))

   /**
    * select the children of the root of a tree
    */
   def getChildren[T[_] : Tree, A] : T[A] =>> T[A] =
      arrL(implicitly[Tree[T]].getChildren)

   /**
    * select the node info of the root of a tree
    */
   def getNode[T[_] : Tree, A] : T[A] =>> A =
      arr(implicitly[Tree[T]].getNode)

   /**
    * select the attribute of the root of a tree
    */
   def hasNode[T[_] : Tree, A](predicate : A ⇒ Boolean) : (T[A] =>> T[A]) =
      guards[T[A], A, T[A]](>>>(getNode, isA(predicate)))(self)

   /**
    * substitute the children of the root of a tree
    */
   def setChildren[T[_] : Tree, A](children : List[T[A]]) : (T[A] =>> T[A]) =
      arr(implicitly[Tree[T]].setChildren(children))

   /**
    * substitute the attribute of the root of a tree
    */
   def setNode[T[_] : Tree, A](n : A) : (T[A] =>> T[A]) =
      arr(implicitly[Tree[T]].setNode(n))

   /**
    * edit the children of the root of a tree
    */
   def changeChildren[T[_] : Tree, A](f : List[T[A]] ⇒ List[T[A]]) : (T[A] =>> T[A]) =
      arr(implicitly[Tree[T]].changeChildren(f) _)

   /**
    * edit the attribute of the root of a tree
    */
   def changeNode[T[_] : Tree, A](f : A ⇒ A) : (T[A] =>> T[A]) =
      arr(implicitly[Tree[T]].changeNode(f) _)

   /**
    * apply an arrow element wise to all children of the root of a tree
    * collect these results and substitute the children with this result
    *
    * example: @ processChildren isText @ deletes all subtrees, for which isText does not hold
    *
    * example: @ processChildren (none \`when\` isCmt) @ removes all children, for which isCmt holds
    */
   def processChildren[T[_] : Tree, A](f : T[A] =>> T[A]) : (T[A] =>> T[A]) = {
      val ev = implicitly[Tree[T]]
      >>>(
         combine(
            getNode,
            listA(>>>(arrL(ev.getChildren[A] _), f))
         ),
         arr(ev.mkTree[A, A] _)
      )
   }

   /**
    * similar to processChildren, but the new children are computed by processing
    * the whole input tree
    *
    * example: @ replaceChildren (deep isText) @ selects all subtrees for which isText holds
    * and substitutes the children component of the root node with this list
    */
   def replaceChildren[T[_] : Tree, A](f : T[A] =>> T[A]) : (T[A] =>> T[A]) = {
      val ev = implicitly[Tree[T]]
      >>>(
         combine(
            getNode,
            listA(f)
         ),
         arr(ev.mkTree[A, A] _)
      )
   }

   /**
    *
    * pronounced \"slash\", meaning g inside f
    *
    * defined as @ f \/> g = f >>> getChildren >>> g @
    *
    * example: @ hasName \"html\" \/> hasName \"body\" \/> hasName \"h1\" @
    *
    * This expression selects
    * all \"h1\" elements in the \"body\" element of an \"html\" element, an expression, that
    * corresponds 1-1 to the XPath selection path \"html\/body\/h1\"
    */
   def \>[T[_] : Tree, A, B, C](a1 : A =>> T[B])(a2 : T[B] =>> C) : (A =>> C) =
      >>>(>>>(a1, getChildren[T, B]), a2)

   /**
    *
    * pronounced \"double slash\", meaning g arbitrarily deep inside f
    *
    * defined as @ f \/\/> g = f >>> getChildren >>> deep g @
    *
    * example: @ hasName \"html\" \/\/> hasName \"table\" @
    *
    * This expression selects
    * all top level \"table\" elements within an \"html\" element, an expression.
    * Attantion: This does not correspond
    * to the XPath selection path \"html\/\/table\". The latter on matches all table elements
    * even nested ones, but @\/\/>@ gives in many cases the appropriate functionality.
    */
   def \\>[T[_] : Tree, A, B, C](a1 : A =>> T[B])(a2 : T[B] =>> C) : (A =>> C) =
      >>>(>>>(a1, getChildren[T, B]), deep(a2))

   /**
    *
    * pronounced \"outside\" meaning f containing g
    *
    * defined as @ f \<\/ g = f \`containing\` (getChildren >>> g) @
    */
   def <\[T[_] : Tree, A](a1 : T[A] =>> T[A])(a2 : T[A] =>> T[A]) : (T[A] =>> T[A]) =
      containing(a1)(>>>(getChildren, a2))

   /**
    * recursively searches a whole tree for subtrees, for which a predicate holds.
    * The search is performed top down. When a tree is found, this becomes an element of the result
    * list. The tree found is not further examined for any subtress, for which the predicate also could hold.
    * See 'multi' for this kind of search.
    *
    * example: @ deep isHtmlTable @ selects all top level table elements in a document
    * (with an appropriate definition for isHtmlTable) but no tables occuring within a table cell.
    */
   def deep[T[_] : Tree, A, B](a : T[A] =>> B) : (T[A] =>> B) =
      orElse(a)(>>>(getChildren[T, A], deep(a)))

   /**
    * recursively searches a whole tree for subrees, for which a predicate holds.
    * The search is performed bottom up.
    *
    * example: @ deepest isHtmlTable @ selects all innermost table elements in a document
    * but no table elements containing tables. See 'deep' and 'multi' for other search strategies.
    */
   def deepest[T[_] : Tree, A, B](a : T[A] =>> B) : (T[A] =>> B) =
      orElse(>>>(getChildren[T, A], deep(a)))(a)

   /**
    * recursively searches a whole tree for subtrees, for which a predicate holds.
    * The search is performed top down. All nodes of the tree are searched, even within the
    * subtrees of trees for which the predicate holds.
    *
    * example: @ multi isHtmlTable @ selects all table elements, even nested ones.
    */
   def multi[T[_] : Tree, A, B](a : T[A] =>> B) : (T[A] =>> B) =
      <+>(a, >>>(getChildren, multi(a)))

   /**
    * recursively transforms a whole tree by applying an arrow to all subtrees,
    * this is done bottom up depth first, leaves first, root as last tree
    *
    * example: @ processBottomUp (getChildren \`when\` isHtmlFont) @ removes all font tags in a HTML document, even nested ones
    * (with an appropriate definition of isHtmlFont)
    */
   def processBottomUp[T[_] : Tree, A](a : T[A] =>> T[A]) : (T[A] =>> T[A]) =
      >>>(processChildren(processBottomUp(a)), a)

   /**
    * similar to 'processBottomUp', but recursively transforms a whole tree by applying an arrow to all subtrees
    * with a top down depth first traversal strategie. In many cases 'processBottomUp' and 'processTopDown'
    * give same results.
    */
   def processTopDown[T[_] : Tree, A](a : T[A] =>> T[A]) : (T[A] =>> T[A]) =
      >>>(a, processChildren(processTopDown(a)))

   /**
    * recursively transforms a whole tree by applying an arrow to all subtrees,
    * but transformation stops when a predicte does not hold for a subtree,
    * leaves are transformed first
    */
   def processBottomUpWhenNot[T[_] : Tree, A](a : T[A] =>> T[A])(predicate : T[A] =>> T[A]) : (T[A] =>> T[A]) =
      whenNot(>>>(processChildren(processBottomUpWhenNot(a)(predicate)), a))(predicate)

   /**
    * recursively transforms a whole tree by applying an arrow to all subtrees,
    * but transformation stops when a tree is successfully transformed.
    * the transformation is done top down
    *
    * example: @ processTopDownUntil (isHtmlTable \`guards\` tranformTable) @
    * transforms all top level table elements into something else, but inner tables remain unchanged
    */
   def processTopDownUntil[T[_] : Tree, A](a : T[A] =>> T[A]) : (T[A] =>> T[A]) =
      orElse(a)(processChildren(processTopDownUntil(a)))

   /**
    * computes a list of trees by applying an arrow to the input
    * and inserts this list in front of index i in the list of children
    *
    * example: @ insertChildrenAt 0 (deep isCmt) @ selects all subtrees for which isCmt holds
    * and copies theses in front of the existing children
    */
   def insertChildrenAt[T[_] : Tree, A] : Int ⇒ (T[A] =>> T[A]) ⇒ (T[A] =>> T[A]) // TODO implementation

   /**
    * similar to 'insertChildrenAt', but the insertion position is searched with a predicate
    */
   def insertChildrenAfter[T[_] : Tree, A] : (T[A] =>> T[A]) ⇒ (T[A] =>> T[A]) ⇒ (T[A] =>> T[A]) // TODO implementation

   /**
    * an arrow for inserting a whole subtree with some holes in it (a template)
    * into a document. The holes can be filled with contents from the input.
    */
   def insertTreeTemplate[T[_] : Tree, A, B] : (T[A] =>> T[A]) ⇒ IfThen[T[A] =>> B, T[A] =>> T[A]] ⇒ (T[A] =>> T[A]) // TODO implementation
}

object ArrowTree {
   @inline def apply[F[-_, +_]](implicit ev : ArrowTree[F]) : ArrowTree[F] = ev
}
