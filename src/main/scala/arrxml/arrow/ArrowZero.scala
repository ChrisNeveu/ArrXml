package arrxml.arrow

import scalaz.Arrow

import annotation.implicitNotFound

@implicitNotFound(msg = "No instance in scope for ArrowZero[${=>>}].")
trait ArrowZero[=>>[-_, +_]] extends Arrow[=>>] {
   def zeroArrow[A, B] : A =>> B
}

object ArrowZero {
   @inline def apply[F[-_, +_]](implicit ev : ArrowZero[F]) : ArrowZero[F] = ev
}
