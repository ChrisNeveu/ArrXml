package arrxml.arrow

import scalaz.Arrow

trait ArrowPlus[=>>[-_, +_]] extends ArrowZero[=>>] {
   def <+>[A, B](arr1 : A =>> B, arr2 : A =>> B) : A =>> B
}
