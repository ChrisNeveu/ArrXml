package arrxml.arrow

import scalaz.Arrow

trait ArrowApply[=>>[-_, +_]] extends Arrow[=>>] {
   def app[A, B] : (A =>> B, A) =>> B
}

object ArrowApply {
   @inline def apply[F[-_, +_]](implicit ev : ArrowApply[F]) : ArrowApply[F] = ev
}
