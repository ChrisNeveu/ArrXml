package arrxml

import scalaz.Arrow

class Process[-A, +B] private (val run : A ⇒ List[B]) extends AnyVal

object Process {
   def apply[A] = new Process[A, A](a ⇒ List(a))
   def apply[A, B](f : A ⇒ List[B]) = new Process(f)

   val arrowInstance = new Arrow[Process] {
      def arr[A, B](f : (A) ⇒ B) : Process[A, B] =
         Process(a ⇒ List(f(a)))
      def first[A, B, C](f : Process[A, B]) : Process[(A, C), (B, C)] =
         Process({
            case (a, c) ⇒ for (b ← f.run(a)) yield (b, c)
         })
      def id[A] : Process[A, A] = Process(List(_))
      def compose[A, B, C](f : Process[B, C], g : Process[A, B]) : Process[A, C] =
         ???
   }
}
