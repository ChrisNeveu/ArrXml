package arrxml.arrow

import annotation.implicitNotFound

@implicitNotFound(msg = "No instance in scope for ArrowList[${=>>}].")
trait ArrowList[=>>[-_, +_]] extends ArrowPlus[=>>] with ArrowApply[=>>] {

   /**
    * construction of a 2 argument arrow from a binary function
    *
    * example: @ a1 &&& a2 >>> arr2 f @
    */
   def arr[A1, A2, B](f : (A1, A2) ⇒ B) : (A1, A2) =>> B =
      arr(tup ⇒ f(tup._1, tup._2))

   /**
    * construction of a 3 argument arrow from a 3-ary function
    *
    * example: @ a1 &&& a2 &&& a3 >>> arr3 f @
    */
   def arr[A1, A2, A3, B](f : (A1, A2, A3) ⇒ B) : (A1, (A2, A3)) =>> B =
      arr(tup ⇒ f(tup._1, tup._2._1, tup._2._2))

   /**
    * construction of a 4 argument arrow from a 4-ary function
    *
    * example: @ a1 &&& a2 &&& a3 &&& a4 >>> arr4 f @
    */
   def arr[B1, B2, B3, B4, C](f : (B1, B2, B3, B4) ⇒ C) : (B1, (B2, (B3, B4))) =>> C =
      arr(tup ⇒ f(tup._1, tup._2._1, tup._2._2._1, tup._2._2._2))

   /**
    * construction of a 2 argument arrow from a single argument arrow
    */
   def arr2A[A, B, C](f : A ⇒ B =>> C) : (A, B) =>> C =
      >>>(first(arr(f)), app[B, C])

   /**
    * constructor for a list arrow from a function with a list as result
    */
   def arrL[A, B](f : A ⇒ List[B]) : A =>> B

   /**
    * constructor for a list arrow with 2 arguments
    */
   def arrL[A, B, C](f : (A, B) ⇒ List[C]) : (A, B) =>> C =
      arrL(tup ⇒ f(tup._1, tup._2))

   /**
    * constructor for a const arrow: @ constA = arr . const @
    */
   def constA[A, B](b : B) : A =>> B = arr(_ ⇒ b)

   /**
    * constructor for a const arrow: @ constL = arrL . const @
    */
   def constL[A, B](bs : List[B]) : A =>> B = arrL(_ ⇒ bs)

   /**
    * builds an arrow from a predicate.
    * If the predicate holds, the single list containing the input is returned, else the empty list
    */
   def isA[B](cond : B ⇒ Boolean) : B =>> B

   /**
    * combinator for converting the result of a list arrow into another list
    *
    * example: @ foo >>. reverse @ reverses the the result of foo
    *
    * example: @ foo >>. take 1 @ constructs a deterministic version of foo by deleting all further results
    */
   def >>%[A, B, C](a : A =>> B, f : List[B] ⇒ List[C]) : A =>> C

   /**
    * combinator for converting the result of an arrow into a single element result
    */
   def >%[A, B, C](a : A =>> B, f : List[B] ⇒ C) : A =>> C =
      >>%(a, ((bs : List[B]) ⇒ List(f(bs))))

   /**
    * combinator for converting an arrow into a determinstic version with all results collected in a single element list
    *
    * @ listA af = af >>. (:[]) @
    *
    * this is useful when the list of results computed by an arrow must be manipulated (e.g. sorted)
    *
    * example for sorting the results of a filter
    *
    * > collectAndSort         :: a b c -> a b c
    * >
    * > collectAndSort collect = listA collect >>> arrL sort
    */
   def listA[A, B](a : A =>> B) : A =>> List[B] =
      >>%(a, (bs : List[B]) ⇒ List(bs))

   /**
    * the inverse of 'listA'
    *
    * @ listA af >>> unlistA = af @
    *
    * unlistA is defined as @ arrL id @
    */
   def unlistA[B] : List[B] =>> B = arrL(a ⇒ a)

   /**
    * the identity arrow, alias for returnA
    */
   def self[B] : B =>> B = arr(a ⇒ a)

   /**
    * the zero arrow, alias for zeroArrow
    */
   def none[B, C] : B =>> C = zeroArrow

   /**
    * converts an arrow, that may fail, into an arrow that always succeeds
    *
    * example: @ withDefault none \"abc\" @ is equivalent to @ constA \"abc\" @
    */
   def withDefault[A, B](a : A =>> B, b : B) : A =>> B =
      >>%(a, (bs : List[B]) ⇒ if (bs.isEmpty) List(b) else bs)

   /**
    * makes a list arrow deterministic, the number of results is at most 1
    *
    * definition
    *
    * > single f = f >>. take 1
    *
    * examples with strings:
    *
    * > runLA ( single none ) "x" == []
    * > runLA ( single this ) "x" == ["x"]
    * > runLA ( single
    * >         (constA "y"
    * >          <+> this ) ) "x" == ["y"]
    */
   def single[A, B](a : A =>> B) : A =>> B = >>%(a, (bs : List[B]) ⇒ bs.headOption.toList)

   /*
    * compute an arrow from the input and apply the arrow to this input
    *
    * definition: @ (f &&& this) >>> app @
    *
    * in a point free style, there is no way to use an argument in 2 places,
    * this is a combinator for simulating this. first the argument is used to compute an arrow,
    * then this new arrow is applied to the input
    *
    * applyA coresponds to: @ apply f x = let g = f x in g x @
    *
    * see also: '$<', '$<<', '$<<<', '$<<<<', '$<$'
*/
   def applyA[A, B](a : A =>> (A =>> B)) : A =>> B =
      >>>(combine(a, self), app[A, B])

   /**
    * compute the parameter for an arrow with extra parameters from the input
    * and apply the arrow for all parameter values to the input
    *
    * a kind of \"function call\" for arrows, useful for joining arrows
    *
    * > infixl 2 ($<)
    *
    * definition:
    *
    * > g $< f = applyA (f >>> arr g)
    *
    * if @f@ fails, the whole arrow fails, e.g. @ g \$\< none == none @
    *
    * if @f@ computes n values and @g@ is deterministic, the whole arrow computes n values
    *
    * examples with simple list arrows with strings
    *
    * > prefixString   :: String -> a String String
    * > prefixString s =  arr (s++)
    * >
    * > runLA ( prefixString $< none           ) "x" == []
    * > runLA ( prefixString $< constA "y"     ) "x" == ["yx"]
    * > runLA ( prefixString $< this           ) "x" == ["xx"]
    * > runLA ( prefixString $< constA "y"
    * >                         <+> constA "z" ) "x" == ["yx","zx"]
    * > runLA ( prefixString $< constA "y"
    * >                         <+> this
    * >                         <+> constA "z" ) "x" == ["yx","xx","zx"]
    *
    * see also: 'applyA', '$<<', '$<<<', '$<<<<', '$<$'
    */
   def %<[A, B, C](f : B ⇒ A =>> C, a : A =>> B) : A =>> C =
      applyA(>>>(a, arr(f)))

   /**
    * binary version of '$<'
    *
    * example with simple list arrows with strings
    *
    * > infixString    :: String -> String -> a String String
    * > infixString s1 s2
    * >                = arr (\ s -> s1 ++ s ++ s2)
    * >
    * > runLA ( infixString $<< constA "y" &&& constA "z" ) "x" = ["yxz"]
    * > runLA ( infixString $<< this &&& this             ) "x" = ["xxx"]
    * > runLA ( infixString $<< constA "y"
    * >                         &&& (constA "z" <+> this) ) "x" = ["yxz", "yxx"]
    */
   def %<[B1, B2, A, C](f : (B1, B2) ⇒ A =>> C, a : A =>> (B1, B2)) : A =>> C =
      applyA(>>>(a, arr(f)))

   /**
    * version of '$<' for arrows with 3 extra parameters
    *
    * typical usage
    *
    * > f $<<< g1 &&& g2 &&& g3
    */
   def %<[B1, B2, B3, A, C](f : (B1, B2, B3) ⇒ A =>> C, a : A =>> (B1, (B2, B3))) : A =>> C =
      applyA(>>>(a, arr(f)))

   /**
    * version of '$<' for arrows with 4 extra parameters
    *
    * typical usage
    *
    * > f $<<<< g1 &&& g2 &&& g3 &&& g4
    */
   def %<[B1, B2, B3, B4, A, C](f : (B1, B2, B3, B4) ⇒ A =>> C, a : A =>> (B1, (B2, (B3, B4)))) : A =>> C =
      applyA(>>>(a, arr(f)))

   /**
    * compute the parameter for an arrow @f@ with an extra parameter by an arrow @g@
    * and apply all the results from @g@ sequentially to the input
    *
    * > infixl 2 ($<$)
    *
    * typical usage:
    *
    * > g :: a b c
    * > g = ...
    * >
    * > f :: c -> a b b
    * > f x = ... x ...
    * >
    * > f $<$ g
    *
    * @f@ computes the extra parameters for @g@ from the input of type @b@ and @g@ is applied with this
    * parameter to the input. This allows programming in a point wise style in @g@, which becomes
    * neccessary, when a value is needed more than once.
    *
    * this combinator is useful, when transforming a single value (document) step by step,
    * with @g@ for collecting the data for all steps, and @f@ for transforming the input step by step
    *
    * if @g@ is deterministic (computes exactly one result),
    * @ g $\<$ f == g $\< f @ holds
    *
    * if @g@ fails, @ f $<$ g == this @
    *
    * if @g@ computes more than one result, @f@ is applied sequentially to the input for every result from @g@
    *
    * examples with simple list arrows with strings
    *
    * > prefixString   :: String -> a String String
    * > prefixString s =  arr (s++)
    * >
    * > runLA ( prefixString $<$ none                      ) "x" == ["x"]
    * > runLA ( prefixString $<$ constA "y"                ) "x" == ["yx"]
    * > runLA ( prefixString $<$ constA "y" <+> constA "z" ) "x" == ["zyx"]
    * > runLA ( prefixString $<$ constA "y" <+> this
    * >                          <+> constA "z"            ) "x" == ["zxyx"]
    *
    * example with two extra parameter
    *
    * > g1 :: a b c1
    * > g2 :: a b c2
    * >
    * > f          :: (c1, c2) -> a b b
    * > f (x1, x2) =  ... x1 ... x2 ...
    * >
    * > f $<$ g1 &&& g2
    *
    * see also: 'applyA', '$<'
    */
   def %<%[A, B](f : B ⇒ (A =>> A), a : A =>> B) : A =>> A =
      applyA(>>>(listA(>>>(a, arr(f))), arr(seqA[A] _)))

   /**
    * merge the result pairs of an arrow with type @a a1 (b1, b2)@
    * by combining the tuple components with the @op@ arrow
    *
    * examples with simple list arrows working on strings and XmlTrees
    *
    * >     a1 :: a String (XmlTree, XmlTree)
    * >     a1 = selem "foo" [this >>> mkText]
    * >          &&&
    * >          selem "bar" [arr (++"0") >>> mkText]
    * >
    * >     runLA (a1 >>> mergeA (<+>) >>> xshow this) "42" == ["<foo>42</foo>","<bar>420</bar>"]
    * >     runLA (a1 >>> mergeA (+=)  >>> xshow this) "42" == ["<foo>42<bar>420</bar></foo>"]
    *
    * see also: 'applyA', '$<' and '+=' in class 'Text.XML.HXT.Arrow.ArrowXml'
    */
   def mergeA[A1, B1, C](f : (A1, B1) =>> A1 ⇒ (A1, B1) =>> B1 ⇒ (A1, B1) =>> C) : (A1, B1) =>> C =
      %<(((a : (A1, B1)) ⇒ f(arr((tup : (A1, B1)) ⇒ tup._1))(constA(a._2))), self)

   /**
    * useful only for arrows with side effects: perform applies an arrow to the input
    * ignores the result and returns the input
    *
    * example: @ ... >>> perform someTraceArrow >>> ... @
    */
   def perform[A, B](a : A =>> B) : A =>> A =
      >>>(combine(listA(a), self[A]), arr((p : (List[B], A)) ⇒ p._2))

   /**
    * generalization of arrow combinator '<+>'
    *
    * definition: @ catA = foldl (\<+\>) none @
    */
   def catA[A, B](as : List[A =>> B]) : A =>> B =
      as.foldLeft(none[A, B])(<+>[A, B] _)

   /**
    * generalization of arrow combinator '>>>'
    *
    * definition: @ seqA = foldl (>>>) this @
    */
   def seqA[B](as : List[B =>> B]) : B =>> B =
      as.foldLeft(self[B])(>>>[B, B, B] _)
}

object ArrowList {
   @inline def apply[F[-_, +_]](implicit ev : ArrowList[F]) : ArrowList[F] = ev
}

trait ToArrowListOps {
   implicit class ArrowListOps[F[-_, +_], A, B](v : F[A, B]) {
      final def >>%[C](f : List[B] ⇒ List[C])(implicit ev : ArrowList[F]) = ev.>>%(v, f)
      final def >%[C](f : List[B] ⇒ C)(implicit ev : ArrowList[F]) = ev.>%(v, f)
      final def withDefault(b : B)(implicit ev : ArrowList[F]) = ev.withDefault(v, b)
   }
}

trait ToArrowListFuncOps {
   implicit class ArrowListFuncOps0[=>>[-_, +_], A, B, C](v : B ⇒ A =>> C) {
      def %<(a : A =>> B)(implicit ev : ArrowList[=>>]) = ev.%<(v, a)
   }
   implicit class ArrowListFuncOps1[=>>[-_, +_], A, B1, B2, C](v : (B1, B2) ⇒ A =>> C) {
      def %<(a : A =>> (B1, B2))(implicit ev : ArrowList[=>>]) = ev.%<(v, a)
   }
   implicit class ArrowListFuncOps2[=>>[-_, +_], A, B1, B2, B3, C](v : (B1, B2, B3) ⇒ A =>> C) {
      def %<(a : A =>> (B1, (B2, B3)))(implicit ev : ArrowList[=>>]) = ev.%<(v, a)
   }
   implicit class ArrowListFuncOps3[=>>[-_, +_], A, B1, B2, B3, B4, C](v : (B1, B2, B3, B4) ⇒ A =>> C) {
      def %<(a : A =>> (B1, (B2, (B3, B4))))(implicit ev : ArrowList[=>>]) = ev.%<(v, a)
   }
   implicit class ArrowListFuncOps4[=>>[-_, +_], A, B](v : B ⇒ (A =>> A)) {
      def %<(a : A =>> B)(implicit ev : ArrowList[=>>]) = ev.%<(v, a)
   }
}
