package arrxml.arrow

import annotation.implicitNotFound

@implicitNotFound(msg = "No instance in scope for Tree[${T}].")
trait Tree[T[_]] {
   /**
    * construct a leaf
    */
   def mkLeaf[A] : A ⇒ T[A]

   /**
    * construct an inner node
    */
   def mkTree[A, B] : A ⇒ List[T[A]] ⇒ T[A]

   /**
    * leaf test: list of children empty?
    */
   def isLeaf[A](tree : T[A]) : Boolean

   /**
    * innner node test: @ not . isLeaf @
    */
   def isInner[A](tree : T[A]) : Boolean

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
   def changeChildren[A](f : T[A] ⇒ T[A])(tree : T[A]) : T[A]

   /**
    * substitute node: @ setNode n = changeNode (const n) @
    */
   def setNode[A](v : A)(tree : T[A]) : T[A]

   /**
    * substitute children: @ setChildren cl = changeChildren (const cl) @
    */
   def setChildren[A](v : T[A])(tree : T[A]) : T[A]

   /**
    * fold for trees
    */
   def foldTree[A, B](f : A ⇒ List[B] ⇒ B)(tree : T[A]) : B

   /**
    * all nodes of a tree
    */
   def nodesTree[A](tree : T[A]) : List[A]

   /**
    * depth of a tree
    */
   def depthTree[A](tree : T[A]) : Int

   /**
    * number of nodes in a tree
    */
   def cardTree[A](tree : T[A]) : Int

   /**
    * format tree for readable trace output
    *
    *  a /graphical/ representation of the tree in text format
    */
   def formatTree[A](f : A ⇒ String)(tree : T[A]) : String
}

object Tree {
   @inline def apply[F[_]](implicit ev : Tree[F]) : Tree[F] = ev
}
