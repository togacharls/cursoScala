package com.github.scouto.sesion5

import scala.annotation.tailrec

/**
  * Created by scouto.
  */
sealed trait Lista[+A]
case object Vacio extends Lista[Nothing]
case class Cons[+A](head:A, tail:Lista[A]) extends Lista[A]

object Lista {


  def apply[A](as: A*): Lista[A] =
    if (as.isEmpty) Vacio
    else Cons(as.head, apply(as.tail: _*))


  def sum(ints: Lista[Int]): Int = {
    ints match {
      case Vacio => 0
      case Cons(h,t) => h + sum(t)
    }
  }

  def product(ints: Lista[Double]) : Double = {
    ints match {
      case Vacio => 1.0
      case Cons(h,t) => h * product(t)
    }
  }

  def tail[A](list: Lista[A]): Lista[A] = {
    list match {
      case Vacio => Vacio
      case Cons(_,t) => t
    }
  }

  def setHead[A](list: Lista[A], newHead: A): Lista[A] = {
    list match  {
      case Vacio => Lista(newHead)
      case Cons(h,t) => Cons(newHead, t)
    }

  }

  def drop[A](list: Lista[A],n: Int): Lista[A] = {

    @tailrec
    def go (rest: Lista[A], x: Int) : Lista[A] = {
      (rest, x) match {
        case (Vacio, _) => Vacio
        case (_, current) if current <= 0 => rest
        case (Cons(_,t), current) if current > 0 =>  go(t, x-1)
      }
    }

    go (list, n)

  }

  def dropWhile[A](list: Lista[A])(f: A => Boolean): Lista[A] = {

    @tailrec
    def go (rest: Lista[A]) : Lista[A] = {
      rest match {
        case Vacio => Vacio
        case Cons(h,t) if f(h) => go(t)
        case Cons(h,_) if !f(h) => rest
      }
    }

    go (list)
  }

  def append[A](l1: Lista[A], l2: Lista[A]): Lista[A] = {

    l1 match {
      case Vacio => l2
      case Cons(h,t) => Cons (h, append(t, l2))
    }
  }

  def init[A](l: Lista[A]): Lista[A] = {

    @tailrec
    def loop(acc: Lista[A], rest: Lista[A]): Lista[A] = {

      rest match {
        case Vacio => acc
        case Cons(_, Vacio) => acc
        case Cons(h, t) => loop(append(acc, Lista(h)), t)
      }

//      rest match {
//        case Vacio => acc
//        case Cons(h, t) => t match {
//          case Vacio => acc
//          case Cons(h, t) => loop(append(acc, Lista(h)), t)
//        }
//      }
    }
    loop(Lista(), l)
  }

  // -------------------- Sesion 6 --------------------

  def foldRight[A,B](as: Lista[A], z: B) (f: (A, B) => B) : B = ???

  def sumFold(ints: Lista[Int]): Int = ???

  def productFold(ints: Lista[Double]) : Double = ???

  def length[A](as: Lista[A]): Int = ???

  //@tailrec
  def foldLeft[A,B](as: Lista[A], z: B) (f: (B, A) => B) : B = ???

  def sumFoldLeft(ints: Lista[Int]): Int = ???

  def productFoldLeft(ints: Lista[Double]) : Double = ???

  def lengthFoldLeft[A](as: Lista[A]): Int = ???

  def reverse[A](as: Lista[A]): Lista [A] = ???

  def foldRightbyLeft[A,B](as: Lista[A], z: B) (f: (A, B) => B) : B = ???

  def foldLeftbyRight[A,B](as: Lista[A], z: B) (f: (B, A) => B) : B = ???

  def productFoldRightLeft(ints: Lista[Double]) : Double = ???

  def productFoldLeftRight(ints: Lista[Double]) : Double = ???

  def lengthLeftRight[A](as: Lista[A]): Int = ???

  def lengthRightLeft[A](as: Lista[A]): Int = ???

  def appendFoldRight[A](a1: Lista[A], a2: Lista[A]): Lista[A] = ???

  def appendLists[A](as: Lista[Lista[A]]): Lista[A] = ???

  // -------------------- Sesion 7 --------------------

  def addOne(l: Lista[Int]): Lista[Int] = ???

  def doubleToString(l: Lista[Double]): Lista[String] = ???

  def map[A, B](l: Lista[A])(f: A => B): Lista[B] = ???

  def filter[A](l: Lista[A])(f: A => Boolean): Lista[A] = ???

  def flatMap[A, B](l: Lista[A])(f: A => Lista[B]): Lista[B] = ???

  def filterFlatMap[A](l: Lista[A])(f: A => Boolean): Lista[A] = ???

  def tieneSubsecuencia[A](lista: Lista[A], sub: Lista[A]): Boolean = ???

}



