/* INSTRUCTIONS
 *
 * Complete the exercises below.  For each "EXERCISE" comment, add
 * code immediately below the comment.
 *
 * HINT: type definitions into the Scala interpreter to try them out,
 * then paste working code into this file.
 *
 * GRADING
 *
 * 1. Submissions must compile without errors.  Submissions that do not compile
 *    without errors will receive 0 points.
 *
 * 2. Do not modify the "main" method.  Submissions with modified "main" methods will
 *    receive 0 points.
 *
 * 3. You must not use while loops or (re)assignment to variables.
 */

object fp2 {
  // EXERCISE 1: complete the following recursive definition of a "map" function
  // for Scala's builtin List type.  You must not use the builtin "map" method.
  def map [X,Y] (xs:List[X], f:X=>Y) : List[Y] =  xs match {
    case List() => List()
	case y :: ys => (f (y)) :: map(ys, f)
  }
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 2: complete the following recursive definition of a "filter" function
  // for Scala's builtin List type.  You must not use the builtin "filter" method.
  def filter [X] (xs:List[X], f:X=>Boolean) : List[X] = xs match {
	case List() => List()
	case (x :: xs) if( f(x)) => x :: (filter(xs, f))
	case (x :: xs)		   => filter(xs, f)
  }
  //----------------------------------------------------------------------------------------------------
  val list1 : List[Int] = (1 to 20).toList
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 3: use "map" to compute a list containing 21..40 from "list1" above.
  // You must use the "map" function that you defined above.
  val list2 : List[Int] = map(list1, (i: Int) => 20 + i) 
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 4: use "map" to compute a list containing 21..40 from "list1" above.
  // You must use the "map" method of Scala's builtin List class.
  val list3 : List[Int] = list1.map((i => 20 + i))
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 5: use "filter" to compute a list containing 3,6,9..18 (numbers divisble
  // by 3) from "list1" above.
  // You must use the "filter" function that you defined above.
  val list4 : List[Int] = filter(list1, (s: Int) => s%3 == 0)
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 6: use "filter" to compute a list containing 3,6,9..18 (numbers divisble
  // by 3) from "list1" above.
  // You must use the "filter" method of Scala's builtin List class.
  val list5 : List[Int] = list1.filter(s => s%3 == 0)
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 7: complete the definition of the following function that computes the length of
  // each String in a list, and returns the original Strings paired with their length.
  // For example:
  //
  //   stringLengths (List ("the", "rain")) == List (("the", 3), ("rain", 4))
  //
  // You must not use recursion directly in the function.  You can use the "map" method
  // of the List class.
  def stringLengths (xs:List[String]) : List[(String, Int)] = xs match {
	case List() => List()
	case (x :: xs)  => ((x, x.length) :: List.map2(xs.map(s=>s), xs.map(s=>s.length))((_, _)))
  }
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 8: complete the function definition for "delete1" that takes
  // an element "x" and a list "ys", then returns the list where any
  // occurrences of "x" in "ys" have been removed.  Your definition of
  // "delete1" MUST be recursive.
  //
  // For example:
  //
  //   delete1 ("the", List ("the","the","was","a","product","of","the","1980s"))
  //   == List ("was","a","product","of","1980s")
  //
  def delete1 [X] (x:X, ys:List[X]) : List[X] = ys match {
	case List() => List()
	case (y :: ys) if(x==y) => (delete1(x, ys))
	case (y :: ys)          => y :: (delete1(x, ys))
  }
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 9: complete the function definition for "delete2" below.  It must
  // have the same behavior as "delete1".  It must be written using "for comprehensions"
  // and not use recursion explicitly.
  def delete2 [X] (x:X, ys:List[X]) : List[X] = for (y <- ys; if(x !=y)) yield y 
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 10: complete the function definition for "delete3" below.  It must
  // have the same behavior as "delete1".  It must be written using the
  // builtin "filter" method for Lists and not use recursion explicitly.
  def delete3 [X] (x:X, ys:List[X]) : List[X] = ys.filter(s => s!=x)
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 11: complete the function definition for "removeDupes1" below.
  // It takes a list as argument, then returns the same list with
  // consecutive duplicate elements compacted to a single element.
  // Duplicate elements that are separated by at least one distinct
  // element should be left alone.
  //
  // For example:
  //
  //   removeDupes1 (List (1,1,2,3,3,3,4,4,5,6,7,7,8,9,2,2,2,9))
  //   == List (1,2,3,4,5,6,7,8,9,2,9)
  //
  def removeDupes1 [X] (xs:List[X]) : List[X] = 
  { 
	def aux [X] (dupeVal:X, ys:List[X]) : List[X] = ys match {
		case List() => 	{
		if (dupeVal !=null) dupeVal :: List()
		else List()
		}
		case y :: ys =>	{
		if(y != dupeVal) dupeVal :: aux(y, ys)
		else aux(y, ys)			
		}
	}
	if(xs.length>0)
	aux(xs.head, xs.tail)
	else
	List()
  }
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 12: write a function "removeDupes2" that behaves like "removeDupes1",
  // but also includes a count of the number of consecutive duplicate
  // elements in the original list (thus producing a simple run-length
  // encoding).  The counts are paired with each element in the output
  // list.
  //
  // For example:
  //
  //   removeDupes2 (List (1,1,2,3,3,3,4,4,5,6,7,7,8,9,2,2,2,9))
  //   == List ((2,1),(1,2),(3,3),(2,4),(1,5),(1,6),(2,7),(1,8),(1,9),(3,2),(1,9))
  //
  def removeDupes2 [X] (xs:List[X]) : List[(Int, X)] = { 
	def aux [X] (dupeVal:X, ys:List[X], counter:Int) : List[(Int, X)] = ys match {
		case List() => 	{
		if (dupeVal !=null) (counter, dupeVal) :: List()
		else List()
		}
		case y :: ys =>	{
		if(y != dupeVal)(counter, dupeVal) :: aux(y, ys, 1)
		else aux(y, ys, counter+1)			
		}
	}
	
	if(xs.length>0)
	aux(xs.head, xs.tail, 1)
	else
	List()
  }
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 13: complete the following definition of a function that splits a list
  // into a pair of two lists.  The offset for the the split position is given
  // by the Int argument.
  // The behavior is determined by:
  //
  //   for all n, xs:
  //     splitAt (n, xs) == (take (n, xs), drop (n, xs))
  //
  // Your definition of "splitAt" must be recursive and must not use "take" or "drop".
  //
  // Your definition of "splitAt" must only travere the list once.  So
  // you cannot define your own versions of "take"/"drop" and use them
  // (because that would entail one traversal of the list with "take"
  // and one with "drop").
   def splitAt [X] (n:Int, xs:List[X]) : (List[X], List[X]) = (n, xs) match {    
    case (_, Nil)       => (Nil, Nil)
	case (-1, list)      => (List(), list)
    case (n, x :: xs)   => {
		val (p1, p2) = splitAt(n - 1, xs)
		if(n>0)		
		(x :: p1, p2)
		else
		(p1, x :: p2)
		}
	}
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 14: Write a recursive function "lineBreak" that takes an Int "n" and a
  // list "xs", then splits "xs" into a list of lists, where each list
  // except the last has length "n".  You should use the builtin "splitAt" from
  // Scala's List class.
  def lineBreak [X] (n:Int, xs:List[X]) : List[List[X]] = xs match {
	case List()  => List()
	case x :: xs => {
		val ll = x::xs
		if(ll.length >= n) List(ll.splitAt(n)._1) ::: lineBreak(n, xs.splitAt(n-1)._2)
		else List(x::xs)
	}
  }	
  //----------------------------------------------------------------------------------------------------
  // EXERCISE 15: Write a recursive function "splitAt2" that differs from "splitAt" by
  // only allowing breaks when a space is seen, i.e., it keeps going
  // when it has seen n characters until it sees the first space.  The
  // trailing space should be placed at the end of the first string in
  // the result pair.  The type is now limited to List[Char]
  //
  // For example:
  //
  //   scala> val (xs1, xs2) = splitAt2 (10, "The rain in Spain falls mainly on the plain.".toList)
  //   xs1: List[Char] = List(T, h, e,  , r, a, i, n,  , i, n,  )
  //   xs2: List[Char] = List(S, p, a, i, n,  , f, a, l, l, s,  , m, a, i, n, l, y,  , o, n,  , t, h, e,  , p, l, a, i, n, .)
  //
  //   scala> println (">>" + new String (xs1.toArray) + "<<")
  //   >>The rain in <<
  //
  //   scala> println (">>" + new String (xs2.toArray) + "<<")
  //   >>Spain falls mainly on the plain.<<
  //
  //   scala> val (xs3, xs4) = splitAt2 (10, xs2)
  //   xs3: List[Char] = List(S, p, a, i, n,  , f, a, l, l, s,  )
  //   xs4: List[Char] = List(m, a, i, n, l, y,  , o, n,  , t, h, e,  , p, l, a, i, n, .)
  //
  //   scala> val (xs5, xs6) = splitAt2 (10, xs4)
  //   xs5: List[Char] = List(m, a, i, n, l, y,  , o, n,  , t, h, e,  )
  //   xs6: List[Char] = List(p, l, a, i, n, .)
  def splitAt2 (n:Int, xs:List[Char]) : (List[Char], List[Char]) = (n, xs) match {
    case (_, Nil)       => (Nil, Nil)
    case (0, list)      => splitAt2(-333, list)
	case (-333, x :: xs)  => 	{
		if(x == ' ') (List(' '), xs)
		else {
		val (p1, p2) = splitAt2(0, xs) 
		(x :: p1, p2) 
		}
	}	
    case (n, x :: xs)   => {
	val (p1, p2) = splitAt2(n - 1, xs)
		if(n>0)		
		(x :: p1, p2)
		else
		(p1, x :: p2)
	}
   }
   
  // EXERCISE 16: Write a new "lineBreak2" that is the same as "lineBreak" but
  // calls "splitAt2" instead of "splitAt".
  //
  // For example:
  //
  //   scala> lineBreak2 (10, "The rain in Spain falls mainly on the plain.".toList)
  //   res6: List[List[Char]] = List(List(T, h, e,  , r, a, i, n,  , i, n,  ), List(S, p, a, i, n,  , f, a, l, l, s,  ), List(m, a, i, n, l, y,  , o, n,  , t, h, e,  ), List(p, l, a, i, n, .))
  //
  //   scala> lineBreak2 (10, "The rain in Spain falls mainly on the plain.".toList).map (xs => new String (xs.toArray)).foreach (s => println (">>" + s + "<<"))
  //   >>The rain in <<
  //   >>Spain falls <<
  //   >>mainly on the <<
  //   >>plain.<<
  def lineBreak2 (n:Int, xs:List[Char]) : List[List[Char]] = xs match {
	case List()  => List()
	case x :: xs => {
		val ll = x::xs
		if(ll.length >= n) List(splitAt2(n, ll)._1) ::: lineBreak2(n, splitAt2(n, ll)._2)
		else List(x::xs)
	}
  }	 
val xss = lineBreak2 (10, "The rain in Spain falls mainly on the plain.".toList)  
  /* ************************************************
   * DO NOT MODIFY ANYTHING BELOW THIS LINE
   * ************************************************ */
   def main (args:Array[String]) = {
     val test1 = List ("The", "rain", "in", "Spain", "falls", "mainly", "on", "the", "plain.")

     try {
       println ("EX1")
       val xss = map (test1, (s:String) => s.toList)
       val yss = map (xss, (xs:List[Char]) => map (xs, (x:Char) => x.isLowerCase))
       println (yss)
       assert (yss.head == List (false, true, true))
     } catch {
       case e => e.printStackTrace
     }

     try {
       println ("EX2")
       val xs = filter (test1, (s:String) => s.length >= 5)
       println (xs)
       assert (xs == List ("Spain", "falls", "mainly", "plain."))
     } catch {
       case e => e.printStackTrace
     }

     try {
       println ("EX3")
       println (list2)
       assert (list2.foldLeft (0) ((x:Int,y:Int)=>x+y) == 610)
       assert (list2.head == 21)
       assert (list2.length == 20)
     } catch {
       case e => e.printStackTrace
     }

     try {
       println ("EX4")
       println (list3)
       assert (list3.foldLeft (0) ((x:Int,y:Int)=>x+y) == 610)
       assert (list3.head == 21)
       assert (list3.length == 20)
     } catch {
       case e => e.printStackTrace
     }

     try {
       println ("EX5")
       println (list4)
       assert (list4.forall ((x:Int) => x % 3 == 0))
       assert (list4.head == 3)
       assert (list4.length == 6)
     } catch {
       case e => e.printStackTrace
     }

     try {
       println ("EX6")
       println (list5)
       assert (list5.forall ((x:Int) => x % 3 == 0))
       assert (list5.head == 3)
       assert (list5.length == 6)
     } catch {
       case e => e.printStackTrace
     }

     try {
       println ("EX7")
       val xs = stringLengths (test1)
       println (xs)
       assert (xs == test1.zip (List (3, 4, 2, 5, 5, 6, 2, 3, 6)))
       assert (stringLengths (List ()) == List ())
     } catch {
       case e => e.printStackTrace
     }

     trait Delete { def delete [X] (x:X, xs:List[X]) : List[X] }

     def testDelete [X] (ex:String, d:Delete) =
       try {
         println (ex)
         val xs = d.delete ("the", List ("the","the","was","a","product","of","the","1980s"))
         println (xs)
         assert (xs == List ("was","a","product","of","1980s"))
         assert (d.delete (1, List ()) == List ())
       } catch {
         case e => e.printStackTrace
       }

     testDelete ("EX8",  new Delete { def delete [X] (x:X, xs:List[X]) = delete1 (x, xs) })
     testDelete ("EX9",  new Delete { def delete [X] (x:X, xs:List[X]) = delete2 (x, xs) })
     testDelete ("EX10", new Delete { def delete [X] (x:X, xs:List[X]) = delete3 (x, xs) })

     val test2 = List (1,1,2,3,3,3,4,4,5,6,7,7,8,9,2,2,2,9)

     try {
       println ("EX11")
       val xs = removeDupes1 (test2)
       println (xs)
       assert (xs == List (1,2,3,4,5,6,7,8,9,2,9))
       assert (removeDupes1 (List ()) == List ())
     } catch {
       case e => e.printStackTrace
     }

     try {
       println ("EX12")
       val xs = removeDupes2 (test2)
       println (xs)
       assert (xs == List ((2,1),(1,2),(3,3),(2,4),(1,5),(1,6),(2,7),(1,8),(1,9),(3,2),(1,9)))
       assert (removeDupes2 (List ()) == List ())
     } catch {
       case e => e.printStackTrace
     }

     try {
       println ("EX13")
       val (xs, ys) = splitAt (7, (1 to 15).toList)
       println (xs)
       println (ys)
       assert (splitAt (7, (1 to 15).toList) == ((1 to 7).toList, (8 to 15).toList))
       assert (splitAt (1, (1 to 15).toList) == ((1 to 1).toList, (2 to 15).toList))
       assert (splitAt (0, (1 to 15).toList) == (List (), (1 to 15).toList))
       assert (splitAt (-1, (1 to 15).toList) == (List (), (1 to 15).toList))
       assert (splitAt (15, (1 to 15).toList) == ((1 to 15).toList, List ()))
       assert (splitAt (16, (1 to 15).toList) == ((1 to 15).toList, List ()))
     } catch {
       case e => e.printStackTrace
     }

     try {
       println ("EX14")
       val xss = lineBreak (4, (1 to 15).toList)
       println (xss)
       assert (xss == List ((1 to 4).toList, (5 to 8).toList, (9 to 12).toList, (13 to 15).toList))
       assert (lineBreak (1, (1 to 15).toList) == (1 to 15).toList.map (x => List (x)))
     } catch {
       case e => e.printStackTrace
     }

     try {
       println ("EX15")
       val (xs1, xs2) = splitAt2 (10, "The rain in Spain falls mainly on the plain.".toList)
       val (xs3, xs4) = splitAt2 (10, xs2)
       val (xs5, xs6) = splitAt2 (10, xs4)
       val (xs7, xs8) = splitAt2 (10, xs6)
       def printListChar (xs:List[Char]) = println (">>" + new String (xs.toArray) + "<<")
       printListChar (xs1)
       printListChar (xs3)
       printListChar (xs5)
       printListChar (xs7)
       assert (xs1 == List('T', 'h', 'e', ' ', 'r', 'a', 'i', 'n', ' ', 'i', 'n', ' '))
       assert (xs3 == List('S', 'p', 'a', 'i', 'n', ' ', 'f', 'a', 'l', 'l', 's', ' '))
       assert (xs5 == List('m', 'a', 'i', 'n', 'l', 'y', ' ', 'o', 'n', ' ', 't', 'h', 'e', ' '))
       assert (xs7 == List('p', 'l', 'a', 'i', 'n', '.'))
     } catch {
       case e => e.printStackTrace
     }

     try {
       println ("EX16")
       val xss = lineBreak2 (10, "The rain in Spain falls mainly on the plain.".toList)
       def printListChar (xs:List[Char]) = println (">>" + new String (xs.toArray) + "<<")
       xss.map (printListChar (_))
       assert (xss == List (List('T', 'h', 'e', ' ', 'r', 'a', 'i', 'n', ' ', 'i', 'n', ' '),
                            List('S', 'p', 'a', 'i', 'n', ' ', 'f', 'a', 'l', 'l', 's', ' '),
                            List('m', 'a', 'i', 'n', 'l', 'y', ' ', 'o', 'n', ' ', 't', 'h', 'e', ' '),
                            List('p', 'l', 'a', 'i', 'n', '.')))
     } catch {
       case e => e.printStackTrace
     }

   }
}
