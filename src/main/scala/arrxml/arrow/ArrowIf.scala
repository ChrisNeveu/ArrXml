package arrxml.arrow

import annotation.implicitNotFound

@implicitNotFound(msg = "No instance in scope for ArrowIf[${=>>}].")
trait ArrowIf[=>>[-_, +_]] extends ArrowList[=>>] {

   /**
    * if lifted to arrows
    */
   def ifA[A, B, C](predicate : A =>> B)(a1 : A =>> C)(a2 : A =>> C) : A =>> C

   /**
    * shortcut: @ ifP p = ifA (isA p) @
    */
   def ifP[A, B](predicate : A ⇒ Boolean)(a1 : A =>> B)(a2 : A =>> B) : A =>> B =
      ifA(isA(predicate))(a1)(a2)

   /**
    * negation: @ neg f = ifA f none this @
    */
   def neg[A, B](a : A =>> B) : A =>> A =
      ifA(a)(none[A, A])(self)

   /**
    * @ f \`when\` g @ : when the predicate g holds, f is applied, else the identity filter this
    */
   def when[A, B](a : A =>> A)(predicate : A =>> B) : (A =>> A) =
      ifA(predicate)(a)(self)

   /**
    * shortcut: @ f \`whenP\` p = f \`when\` (isA p) @
    */
   def whenP[A](a : A =>> A)(predicate : A ⇒ Boolean) : (A =>> A) =
      ifP(predicate)(a)(self)

   /**
    * @ f \`whenNot\` g @ : when the predicate g does not hold, f is applied, else the identity filter this
    */
   def whenNot[A, B](a : A =>> A)(predicate : A =>> B) : (A =>> A) =
      ifA(predicate)(self)(a)

   /**
    * like 'whenP'
    */
   def whenNotP[A](a : A =>> A)(predicate : A ⇒ Boolean) : (A =>> A) =
      ifP(predicate)(self)(a)

   /**
    * @ g \`guards\` f @ : when the predicate g holds, f is applied, else none
    */
   def guards[A, B, C](predicate : A =>> B)(a : A =>> C) : (A =>> C) =
      ifA(predicate)(a)(none[A, C])

   /**
    * like 'whenP'
    */
   def guardsP[A, B](predicate : A ⇒ Boolean)(a : A =>> B) : (A =>> B) =
      ifP(predicate)(a)(none[A, B])

   /**
    * shortcut for @ f `guards` this @
    */
   def filterA[A, B](predicate : A =>> B) : (A =>> A) =
      ifA(predicate)(self)(none[A, A])

   /**
    * @ f \`containing\` g @ : keep only those results from f for which g holds
    *
    * definition: @ f \`containing\` g = f >>> g \`guards\` this @
    */
   def containing[A, B, C](a1 : A =>> B)(a2 : B =>> C) : (A =>> B) =
      >>>(a1, guards(a2)(self))

   /**
    * @ f \`notContaining\` g @ : keep only those results from f for which g does not hold
    *
    * definition: @ f \`notContaining\` g = f >>> ifA g none this @
    */
   def notContaining[A, B, C](a1 : A =>> B)(a2 : B =>> C) : (A =>> B) =
      >>>(a1, ifA(a2)(none[B, B])(self))

   /**
    * @ f \`orElse\` g @ : directional choice: if f succeeds, the result of f is the result, else g is applied
    */
   def orElse[A, B](a1 : A =>> B)(a2 : A =>> B) : (A =>> B) // TODO implementation

   /**
    *
    * generalisation of 'orElse' for multi way branches like in case expressions.
    *
    * An auxiliary data type 'IfThen' with an infix constructor ':->' is used for writing multi way branches
    *
    * example: @ choiceA [ p1 :-> e1, p2 :-> e2, this :-> default ] @
    */
   def choiceA[A, B, C] : List[IfThen[A =>> B, A =>> C]] ⇒ (A =>> C)

   /**
    * tag a value with Left or Right, if arrow has success, input is tagged with Left, else with Right
    */
   def tagA[A, B](a : A =>> B) : (A =>> Either[A, A]) = {
      val left : A ⇒ Either[A, A] = Left.apply[A, A] _
      val right : A ⇒ Either[A, A] = Right.apply[A, A] _

      ifA(a)(arr(left))(arr(right))
   }

   /**
    * split a list value with an arrow and returns a pair of lists.
    * This is the arrow version of 'span'. The arrow is deterministic.
    *
    * example: @ runLA (spanA (isA (\/= \'-\'))) \"abc-def\" @ gives @ [(\"abc\",\"-def\")] @ as result
    */
   def spanA[A](a : A =>> A) : (List[A] =>> (List[A], List[A])) = {
      val predicate = >>>(arrL((l : List[A]) ⇒ l.take(1)), a)
      val split = combine(arr((l : List[A]) ⇒ l.head), >>>(arr((l : List[A]) ⇒ l.tail), spanA(a)))
      val rejoin = arr((tup : (A, (List[A], List[A]))) ⇒ (tup._1 :: tup._2._1, tup._2._2))
      val elseCase = arr((l : List[A]) ⇒ (List.empty[A], l))
      ifA(predicate)(>>>(split, rejoin))(elseCase)
   }

   /**
    * partition a list of values into a pair of lists
    *
    * This is the arrow Version of 'Data.List.partition'
    */
   def partitionA[A](a : A =>> A) : List[A] =>> (List[A], List[A]) = {
      val part = listA(>>>(arrL((as : List[A]) ⇒ as), tagA(a)))
      def untag(l : List[Either[A, A]]) = {
         val (l1, l2) = l.partition(_.isLeft)
         (l1.map(_.fold(((a : A) ⇒ a), ((a : A) ⇒ a))), l2.map(_.fold(((a : A) ⇒ a), ((a : A) ⇒ a))))
      }
      >>>(part, arr(untag _))
   }

}

object ArrowIf {
   @inline def apply[F[-_, +_]](implicit ev : ArrowIf[F]) : ArrowIf[F] = ev
}

trait ToArrowIfOps {
   // For endoarrows.
   implicit class ArrowIfOps0[=>>[-_, +_], A](v : A =>> A) {
      final def when[B](predicate : A =>> B)(implicit ev : ArrowIf[=>>]) = ev.when(v)(predicate)
      final def whenP(predicate : A ⇒ Boolean)(implicit ev : ArrowIf[=>>]) = ev.whenP(v)(predicate)
      final def whenNot[B](predicate : A =>> B)(implicit ev : ArrowIf[=>>]) = ev.whenNot(v)(predicate)
      final def whenNotP(predicate : A ⇒ Boolean)(implicit ev : ArrowIf[=>>]) = ev.whenNotP(v)(predicate)
   }
   implicit class ArrowIfOps1[=>>[-_, +_], A, B](v : A =>> B) {
      final def guards[C](a : A =>> C)(implicit ev : ArrowIf[=>>]) = ev.guards(v)(a)
   }
}

trait ToArrowIfFuncOps {
   implicit class ArrowIfFuncOps[=>>[-_, +_], A](v : A ⇒ Boolean) {
      final def guardsP[B](a : A =>> B)(implicit ev : ArrowIf[=>>]) = ev.guardsP(v)(a)
   }
}

class IfThen[A, B]
