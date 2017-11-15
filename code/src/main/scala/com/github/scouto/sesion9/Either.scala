package com.github.scouto.sesion9

import com.sun.net.httpserver.Authenticator.Success

import scala.util.{Failure, Success, Try}

/**
  * Created by couto on 30/06/17.
  */
sealed trait Either[+E, +A] {
  //Left -> Error/Exception
  //Right -> Valor correcto
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(a) => Left(a)
      case Right(a) => Right(f(a))
    }
  }

  def flatMap [EE >: E, B >: A](f: A => Either[EE, B]): Either[EE, B] = {
    map(f) match {
      case Left(a) => Left(a)
      case Right(a) => a
    }
  }

  def orElse [EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(a) => b
      case Right(a) => Right(a)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) : Either[EE, C] = {
    (this, b) match {
      case(Right(v1), Right(v2)) => Right(f(v1, v2))
      case(Left(error), _) => Left(error)
      case(_, Left(error)) => Left(error)
    }
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {


  def mean(xs:Seq[Double]): Either[String, Double] = {
    if(xs.length == 0) Left("Empty Seq")
    else Right(xs.sum / xs.length)
  }

  def convertToEither(s: String): Either[String, Int] = {
    /*try{
      Right(s.toInt)
    }catch{
      case e:Exception => Left(e.getMessage)
    }*/
    Try(s.toInt) match {
      case Success(ok) => Right(ok)
      case Failure(error) => Left(error.getMessage)
    }
  }

  def calcularCuotaString(age: String, incidencias: String): Either[String, Double] = {
    convertToEither(age).map2(convertToEither(incidencias))(calcularCuota)
  }

  def calcularCuota(age: Int, incidencias: Int): Double = age * incidencias


  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    a.foldRight(Right(Nil): Either[E, List[A]])((elem, acc) => elem.map2(acc)(_::_))
  }

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    a.foldRight(Right(Nil): Either[E, List[B]])((elem, acc) => f(elem).map2(acc)(_::_))
  }

  def sequenceViaTraverse[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(a)(x => x)
  }
}
