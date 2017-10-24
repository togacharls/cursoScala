def removeFirstElement(list: List[Int], f: Int => Boolean): List[Int] = {

  @annotation.tailrec
  def go(acc: List[Int], rest: List[Int]): List[Int] = {

    rest match {
      case Nil => acc
      case h::t if f(h) => acc:::t
      case h::t if !f(h) => go(acc :+ h, t)
    }
  }

  go (List(), list)
}


val l = List(1,2,3,4,5)

removeFirstElement(l, x => x>2)