package arrxml.arrow

import annotation.implicitNotFound

@implicitNotFound(msg = "No instance in scope for ArrowTree[${=>>}].")
trait ArrowTree[=>>[-_, +_]] extends ArrowPlus[=>>] with ArrowIf[=>>] {
   /**
    * construct a leaf
    */
   def mkLeaf[T[_] : Tree, A, B] : A ⇒ (B =>> T[A])

   /**
    * construct an inner node
    */
   def mkTree[T[_] : Tree, A, B] : A ⇒ List[T[A]] ⇒ (B =>> T[A])

   /**
    * select the children of the root of a tree
    */
   def getChildren[T[_] : Tree, A] : T[A] =>> T[A]

   /**
    * select the node info of the root of a tree
    */
   def getNode[T[_] : Tree, A] : T[A] =>> A

   /**
    * select the attribute of the root of a tree
    */
   def hasNode[T[_] : Tree, A] : (A ⇒ Boolean) ⇒ (T[A] =>> T[A])

   /**
    * substitute the children of the root of a tree
    */
   def setChildren[T[_] : Tree, A] : List[T[A]] ⇒ (T[A] =>> T[A])

   /**
    * substitute the attribute of the root of a tree
    */
   def setNode[T[_] : Tree, A] : A ⇒ (T[A] =>> T[A])

   /**
    * edit the children of the root of a tree
    */
   def changeChildren[T[_] : Tree, A] : (List[T[A]] ⇒ List[T[A]]) ⇒ (T[A] ⇒ T[A])

   /**
    * edit the attribute of the root of a tree
    */
   def changeNode[T[_] : Tree, A] : (A ⇒ A) ⇒ (T[A] =>> T[A])

   /**
    * apply an arrow element wise to all children of the root of a tree
    * collect these results and substitute the children with this result
    *
    * example: @ processChildren isText @ deletes all subtrees, for which isText does not hold
    *
    * example: @ processChildren (none \`when\` isCmt) @ removes all children, for which isCmt holds
    */
   def processChildren[T[_] : Tree, A] : (T[A] =>> T[A]) ⇒ (T[A] =>> T[A])

   /**
    * similar to processChildren, but the new children are computed by processing
    * the whole input tree
    *
    * example: @ replaceChildren (deep isText) @ selects all subtrees for which isText holds
    * and substitutes the children component of the root node with this list
    */
   def replaceChildren[T[_] : Tree, A] : (T[A] =>> T[A]) ⇒ (T[A] =>> T[A])

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
   def \>[T[_] : Tree, A, B, C] : (A =>> T[B]) ⇒ (T[B] =>> C) ⇒ (A =>> C)

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
   def \\>[T[_] : Tree, A, B, C] : (A =>> T[B]) ⇒ (T[B] =>> C) ⇒ (A =>> C)

   /**
    *
    * pronounced \"outside\" meaning f containing g
    *
    * defined as @ f \<\/ g = f \`containing\` (getChildren >>> g) @
    */
   def <\[T[_] : Tree, A] : (T[A] =>> T[A]) ⇒ (T[A] =>> T[A]) ⇒ (T[A] =>> T[A])

   /**
    * recursively searches a whole tree for subtrees, for which a predicate holds.
    * The search is performed top down. When a tree is found, this becomes an element of the result
    * list. The tree found is not further examined for any subtress, for which the predicate also could hold.
    * See 'multi' for this kind of search.
    *
    * example: @ deep isHtmlTable @ selects all top level table elements in a document
    * (with an appropriate definition for isHtmlTable) but no tables occuring within a table cell.
    */
   def deep[T[_] : Tree, A, B] : (T[A] =>> B) ⇒ (T[A] =>> B)

   /**
    * recursively searches a whole tree for subrees, for which a predicate holds.
    * The search is performed bottom up.
    *
    * example: @ deepest isHtmlTable @ selects all innermost table elements in a document
    * but no table elements containing tables. See 'deep' and 'multi' for other search strategies.
    */
   def deepest[T[_] : Tree, A, B] : (T[A] =>> B) ⇒ (T[A] =>> B)

   /**
    * recursively searches a whole tree for subtrees, for which a predicate holds.
    * The search is performed top down. All nodes of the tree are searched, even within the
    * subtrees of trees for which the predicate holds.
    *
    * example: @ multi isHtmlTable @ selects all table elements, even nested ones.
    */
   def multi[T[_] : Tree, A, B] : (T[A] =>> B) ⇒ (T[A] =>> B)

   /**
    * recursively transforms a whole tree by applying an arrow to all subtrees,
    * this is done bottom up depth first, leaves first, root as last tree
    *
    * example: @ processBottomUp (getChildren \`when\` isHtmlFont) @ removes all font tags in a HTML document, even nested ones
    * (with an appropriate definition of isHtmlFont)
    */
   def processBottomUp[T[_] : Tree, A] : (T[A] =>> T[A]) ⇒ (T[A] =>> T[A])

   /**
    * similar to 'processBottomUp', but recursively transforms a whole tree by applying an arrow to all subtrees
    * with a top down depth first traversal strategie. In many cases 'processBottomUp' and 'processTopDown'
    * give same results.
    */
   def processTopDown[T[_] : Tree, A] : (T[A] =>> T[A]) ⇒ (T[A] =>> T[A])

   /**
    * recursively transforms a whole tree by applying an arrow to all subtrees,
    * but transformation stops when a predicte does not hold for a subtree,
    * leaves are transformed first
    */
   def processBottomUpWhenNot[T[_] : Tree, A] : (T[A] =>> T[A]) ⇒ (T[A] =>> T[A]) ⇒ (T[A] =>> T[A])

   /**
    * recursively transforms a whole tree by applying an arrow to all subtrees,
    * but transformation stops when a tree is successfully transformed.
    * the transformation is done top down
    *
    * example: @ processTopDownUntil (isHtmlTable \`guards\` tranformTable) @
    * transforms all top level table elements into something else, but inner tables remain unchanged
    */
   def processTopDownUntil[T[_] : Tree, A] : (T[A] =>> T[A]) ⇒ (T[A] =>> T[A])

   /**
    * computes a list of trees by applying an arrow to the input
    * and inserts this list in front of index i in the list of children
    *
    * example: @ insertChildrenAt 0 (deep isCmt) @ selects all subtrees for which isCmt holds
    * and copies theses in front of the existing children
    */
   def insertChildrenAt[T[_] : Tree, A] : Int ⇒ (T[A] =>> T[A]) ⇒ (T[A] =>> T[A])

   /**
    * similar to 'insertChildrenAt', but the insertion position is searched with a predicate
    */
   def insertChildrenAfter[T[_] : Tree, A] : (T[A] =>> T[A]) ⇒ (T[A] =>> T[A]) ⇒ (T[A] =>> T[A])

   /**
    * an arrow for inserting a whole subtree with some holes in it (a template)
    * into a document. The holes can be filled with contents from the input.
    */
   def insertTreeTemplate[T[_] : Tree, A, B] : (T[A] =>> T[A]) ⇒ IfThen[T[A] =>> B, T[A] =>> T[A]] ⇒ (T[A] =>> T[A])
}

object ArrowTree {
   @inline def apply[F[-_, +_]](implicit ev : ArrowTree[F]) : ArrowTree[F] = ev
}
