package com.github.scouto.sesion9

/**
  * Created by couto on 30/06/17.
  */
sealed trait Either[+E, +A] {



  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }
  }

  def flatMap [EE >: E, B >: A](f: A => Either[EE, B]): Either[EE, B] = {
//    this match {
//      case Left(e) => Left(e)
//      case Right(a) => f(a)
//    }
//
    map(f) match {
      case Left(e) => Left(e)
      case Right(x) => x
    }

  }


  def orElse [EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(_) => this
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) : Either[EE, C] = {
    (this, b) match {
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    }


    //this.flatMap(a => b.map(bb => f(a, bb)))
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {


  def mean(xs:Seq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("Empty Seq")
    else Right(xs.sum/xs.length)
  }

  def calcularCuotaString(age: String, incidencias: String): Either[Exception, Double] = {

    def intenta[A](a: A): Either[Exception, A] = {
      try Right(a)
      catch {case e: Exception => Left(e)}
    }

    for {
      a <- intenta{age.toInt}
      i <- intenta(incidencias.toInt)
    }yield calcularCuota(a, i)

    intenta(age.toInt).map2(intenta(incidencias.toInt))(calcularCuota)

  }

  def calcularCuota(age: Int, incidencias: Int): Double = {
    age * incidencias
  }


  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
//    a match {
//      case Nil => Right(Nil)
//      case h::t => h.map2(sequence(t))(_::_)
//    }

    a.foldRight(Right(Nil): Either[E, List[A]])((elem, acc) => elem.map2(acc)(_ :: _))
  }

  def sequenceViaTraverse[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(a)(x => x)
  }

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
//    a match {
//      case Nil => Right(Nil)
//      case h::t => f(h).map2(traverse(t)(f))(_::_)
//    }

    a.foldRight(Right(Nil): Either[E, List[B]])((elem, acc) => f(elem).map2(acc)(_ :: _))

  }
}
