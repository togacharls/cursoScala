package com.github.scouto.sesion8

/**
  * Created by couto
  */
sealed trait Option[+A] {

  def map[B] (f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def flatMap[B](f: A => Option[B]) : Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A] (default: => B): B = {
    this match{
      case None => default
      case Some(v) => v
    }
  }

  //devuelve el valor del Option si existe, en caso contrario devuelve el parámetro recibido
  def orElse[B >: A] (ob: => Option[B]): Option[B] = {
    map(Some(_)) getOrElse ob
    /*this match {
      case None => ob
      case _ => this
    }*/
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]



object Option {


  def mean(xs:Seq[Double]): Option[Double] = {
    if(xs.length == 0) return None
    else Some(xs.foldLeft(0.0)(_+_) / xs.length)
    //else Some(xs.sum / xs.length)
  }

  def calcularCuota(age: Int, incidencias: Int): Double = age * incidencias

  def calcularCuotaString (age: String, incidencias: String): Option[Double] = {
    val myAge = try{
      age.toInt
    }catch {
      case e:Exception => return None
    }

    val myInc = try{
      incidencias.toInt
    }catch {
      case e:Exception => return None
    }

    Some(calcularCuota(myAge, myInc))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (None, None) => None
      case _ => Some(f(a, b))
    }
    a flatMap((aa => b map (bb => f(aa, bb))))
  }

  //List(Some(5), Some(4)) => Some(List(5, 4))
  //List(Some(5), None) => None
  //List() => Some(Nil)
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil): Option[List[A]])((elem, acc) => map2(elem, acc)(_::_))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(Nil): Option[List[B]])((elem, acc) => map2(f(elem), acc)(_::_))
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = ???

  def variance(xs: Seq[Double]): Option[Double] = ???

}