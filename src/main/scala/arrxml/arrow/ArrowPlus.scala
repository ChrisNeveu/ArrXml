package arrxml.arrow

import annotation.implicitNotFound

@implicitNotFound(msg = "No instance in scope for ArrowPlus[${=>>}].")
trait ArrowPlus[=>>[-_, +_]] extends ArrowZero[=>>] {
   def <+>[A, B](arr1 : A =>> B, arr2 : A =>> B) : A =>> B
}

object ArrowPlus {
   @inline def apply[F[-_, +_]](implicit ev : ArrowPlus[F]) : ArrowPlus[F] = ev
}

trait ToArrowPlusOps {
   implicit class ArrowPlusOps[F[-_, +_], A, B](v : F[A, B]) {
      final def <+>(other : F[A, B])(implicit ev : ArrowPlus[F]) = ev.<+>(v, other)
   }
}
