package arrxml.arrow

import scalaz.Arrow
import scalaz.syntax.Ops

trait ArrowPlus[=>>[-_, +_]] extends ArrowZero[=>>] {
   def <+>[A, B](arr1 : A =>> B, arr2 : A =>> B) : A =>> B
}

object ArrowPlus {
   @inline def apply[F[-_, +_]](implicit ev : ArrowPlus[F]) : ArrowPlus[F] = ev
}

trait ArrowPlusOps[F[-_, +_], A, B] extends Ops[F[A, B]] {
   implicit def F : ArrowPlus[F]

   final def <+>(other : F[A, B]) = F.<+>(self, other)
}

trait ToArrowPlusOps {
   implicit def ToArrowPlusOps[F[-_, +_], A, B](v : F[A, B])(implicit ev : ArrowPlus[F]) =
      new ArrowPlusOps[F, A, B] { def self = v; implicit def F : ArrowPlus[F] = ev }
}
