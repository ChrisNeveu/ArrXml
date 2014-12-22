package arrxml.tree

import annotation.implicitNotFound

@implicitNotFound(msg = "No instance in scope for Tree[${T}].")
trait Tree[T[_]] {
   /**
    * construct a leaf
    */
   def mkLeaf[A](a : A) : T[A] = mkTree(a, List.empty)

   /**
    * construct an inner node
    */
   def mkTree[A, B](n : A, children : List[T[A]]) : T[A]

   /**
    * leaf test: list of children empty?
    */
   def isLeaf[A](tree : T[A]) : Boolean = getChildren(tree).isEmpty

   /**
    * innner node test: @ not . isLeaf @
    */
   def isInner[A](tree : T[A]) : Boolean = !isLeaf(tree)

   /**
    * select node attribute
    */
   def getNode[A](tree : T[A]) : A

   /**
    * select children
    */
   def getChildren[A](tree : T[A]) : List[T[A]]

   /**
    * edit node attribute
    */
   def changeNode[A](f : A ⇒ A)(tree : T[A]) : T[A]

   /**
    * edit children
    */
   def changeChildren[A](f : List[T[A]] ⇒ List[T[A]])(tree : T[A]) : T[A]

   /**
    * substitute node: @ setNode n = changeNode (const n) @
    */
   def setNode[A](v : A)(tree : T[A]) : T[A] = changeNode((_ : A) ⇒ v)(tree)

   /**
    * substitute children: @ setChildren cl = changeChildren (const cl) @
    */
   def setChildren[A](children : List[T[A]])(tree : T[A]) : T[A] =
      changeChildren((_ : List[T[A]]) ⇒ children)(tree)

   /**
    * fold for trees
    */
   def foldTree[A, B](f : (A, List[B]) ⇒ B)(tree : T[A]) : B

   /**
    * all nodes of a tree
    */
   def nodesTree[A](tree : T[A]) : List[A] =
      foldTree((a : A, as : List[List[A]]) ⇒ a :: as.flatten)(tree)

   /**
    * depth of a tree
    */
   def depthTree[A](tree : T[A]) : Int =
      foldTree((_ : A, as : List[Int]) ⇒ 1 + (0 :: as).max)(tree)

   /**
    * number of nodes in a tree
    */
   def cardTree[A](tree : T[A]) : Int =
      foldTree((_ : A, as : List[Int]) ⇒ 1 + as.sum)(tree)
}

object Tree {
   @inline def apply[F[_]](implicit ev : Tree[F]) : Tree[F] = ev
}
