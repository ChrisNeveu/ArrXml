package arrxml.arrow

import scalaz.Arrow

trait ArrowZero[=>>[-_, +_]] extends Arrow[=>>] {
   def zeroArrow[A, B] : A =>> B
}
