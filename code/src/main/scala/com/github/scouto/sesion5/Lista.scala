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
//
      rest match {
        case Vacio => acc
        case Cons(_, Vacio) => acc
        case Cons(h, t) => loop(append(acc, Lista(h)), t)
      }


//
//      rest match {
//        case Vacio => acc
//        case Cons(h1, t) => t match {
//          case Vacio => acc
//          case _ => loop(append(acc, Lista(h1)), t)
//        }
//      }
    }
    loop(Lista(), l)
  }

  // -------------------- Sesion 6 --------------------

  def foldRight[A,B](as: Lista[A], z: B) (f: (A, B) => B) : B = {

    as match {
      case Vacio => z
      case Cons (h, t) => f(h, foldRight(t, z)(f))
    }
  }

  def sumFold(ints: Lista[Int]): Int = {
    foldRight(ints, 0)(_ +_ )
  }

  def productFold(ints: Lista[Double]) : Double = {
    foldRight(ints, 1.0)(_ *_ )

  }

  def length[A](as: Lista[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  @tailrec
  def foldLeft[A,B](as: Lista[A], z: B) (f: (B, A) => B) : B = {
    as match {
      case Vacio => z
      case Cons(h,t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sumFoldLeft(ints: Lista[Int]): Int = {
    foldLeft(ints, 0)(_ +_)
  }

  def productFoldLeft(ints: Lista[Double]) : Double = {
    foldLeft(ints, 1.0)(_ * _)
  }

  def lengthFoldLeft[A](as: Lista[A]): Int = {
    foldLeft(as, 0)((acc, _) => acc +1)
  }

  def reverse[A](as: Lista[A]): Lista [A] = {
    foldLeft(as, Lista[A]())((acc, elem) =>  Cons (elem, acc))
//    foldLeft(as, Lista[A]())((acc, elem) =>  append (Lista(elem), acc))
//    foldRight(as, Vacio: Lista[A])((elem, acc) => append(acc, Lista(elem)))
  }

  def foldRightbyLeft[A,B](as: Lista[A], z: B) (f: (A, B) => B) : B = {
    foldLeft(as, z)((acc,elem) => f(elem, acc))
  }

  def foldLeftbyRight[A,B](as: Lista[A], z: B) (f: (B, A) => B) : B = {
    foldRight(as, z)((elem, acc) => f(acc, elem))
  }

  def sumFoldRightLeft(ints: Lista[Int]) : Int = {
    foldRightbyLeft(ints, 0)(_ +_ )
  }

  def sumFoldLeftRight(ints: Lista[Int]) : Int = {
    foldLeftbyRight(ints, 0)(_ +_ )
  }

  def productFoldRightLeft(ints: Lista[Double]) : Double = {
    foldRightbyLeft(ints, 1.0)(_ *_ )
  }

  def productFoldLeftRight(ints: Lista[Double]) : Double = {
    foldLeftbyRight(ints, 1.0)(_ *_ )
  }

  def lengthLeftRight[A](as: Lista[A]): Int = {
    foldLeftbyRight(as, 0)((acc, _) => acc +1)
  }

  def lengthRightLeft[A](as: Lista[A]): Int = {
    foldRightbyLeft(as, 0)((_, acc) => acc +1)
  }

  def appendFoldRight[A](a1: Lista[A], a2: Lista[A]): Lista[A] = {
    foldRight(a1, a2)((elem, acc) => Cons(elem, acc))
  }

  def appendLists[A](as: Lista[Lista[A]]): Lista[A] = {
//    foldRight(as, Vacio: Lista[A])((elem, acc) => appendFoldRight(elem, acc))
//    foldRight(as, Vacio: Lista[A])(appendFoldRight(_, _))
    foldRight(as, Vacio: Lista[A])(appendFoldRight)
  }


  // -------------------- Sesion 7 --------------------

  def addOne(l: Lista[Int]): Lista[Int] = {
//    foldRight(l, Vacio: Lista[Int])((elem, acc) => Cons(elem+1, acc))
    foldLeft(l, Vacio: Lista[Int])((acc, elem) => append(acc,Lista(elem+1)))
  }

  def doubleToString(l: Lista[Double]): Lista[String] = {
//    foldRight(l, Vacio: Lista[String])((elem, acc) => Cons(elem.toString, acc))
        foldLeft(l, Vacio: Lista[String])((acc, elem) => append(acc,Lista(elem.toString)))
  }

  def map[A, B](l: Lista[A])(f: A => B): Lista[B] = {
//    foldRight(l, Vacio: Lista[B])((elem, acc) => Cons(f(elem), acc))
    foldLeft(l, Vacio: Lista[B])((acc, elem) => append(acc,Lista(f(elem))))

  }

  def filter[A](l: Lista[A])(f: A => Boolean): Lista[A] = {
//    foldRight(l, Vacio: Lista[A])((elem, acc) => if (f(elem)) Cons(elem, acc) else acc)
    foldLeft(l, Vacio: Lista[A])((acc, elem) => if (f(elem)) append(acc, Lista(elem)) else acc)
  }

  def flatMap[A, B](l: Lista[A])(f: A => Lista[B]): Lista[B] = {
      appendLists(map(l)(f))
  }

  def filterFlatMap[A](l: Lista[A])(f: A => Boolean): Lista[A] = {
      flatMap(l)(elem => if(f(elem)) Lista(elem) else Vacio)
  }

  def addLists(a1: Lista[Int], a2: Lista[Int]) : Lista[Int] = {

    @tailrec
    def loop(acc: Lista[Int], rest1: Lista[Int], rest2: Lista[Int]): Lista[Int] = {
      (rest1, rest2) match {
        case (Vacio, Vacio) => acc
        case (Vacio, _) => Vacio
        case (_, Vacio) => Vacio
        case (Cons(h1, t1), Cons(h2, t2)) => loop(append(acc, Lista(h1+h2)), t1, t2)
      }


    }
    loop(Vacio, a1, a2)

//    (a1, a2) match {
//      case (Vacio, _) => Vacio
//      case (_, Vacio) => Vacio
//      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addLists(t1, t2))
//    }

  }

  def zipWith[A, B, C](a1: Lista[A], a2: Lista[B])(f: (A, B) => C) : Lista[C] = {

    @tailrec
    def loop(acc: Lista[C], rest1: Lista[A], rest2: Lista[B]): Lista[C] = {
      (rest1, rest2) match {
        case (Vacio, Vacio) => acc
        case (Vacio, _) => Vacio
        case (_, Vacio) => Vacio
        case (Cons(h1, t1), Cons(h2, t2)) => loop(append(acc, Lista(f(h1,h2))), t1, t2)
      }
    }
    loop(Vacio, a1, a2)


//    (a1, a2) match {
//      case (Vacio, _) => Vacio
//      case (_, Vacio) => Vacio
//      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1,t2)(f))
//    }
  }


  def empiezaPor[A](lista: Lista[A], sub: Lista[A]): Boolean = ???



  def tieneSubsecuencia[A](lista: Lista[A], sub: Lista[A]): Boolean = ???

  }



