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
      case Cons(h,t) => t
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

    def loop(acc: Lista[A], rest: Lista[A]): Lista[A] = {

      rest match {
        case Vacio => acc
        case Cons(h, Vacio) => acc
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

  }



