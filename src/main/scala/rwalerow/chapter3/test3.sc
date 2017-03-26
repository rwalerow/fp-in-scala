import rwalerow.chapter3.List.foldRight2
import rwalerow.chapter3.{Cons, List, Nil}

foldRight2(List(1,2,3), Nil: List[Int])(Cons(_, _))