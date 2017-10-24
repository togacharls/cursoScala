package com.github.scouto.sesion4

import scala.annotation.tailrec

/**
  * Created by scouto.
  */
object Sesion4 extends App{

  def isSorted[A](as: List[A])(implicit ordered: (A,A) => Boolean): Boolean = {

    @tailrec
    def loop (rest: List[A]): Boolean = {
      rest match {
        case Nil => true
        case h::Nil => true
        case h1::t if ordered(h1, t.head) => loop (t)
        case h1::t if !ordered(h1, t.head) => false
      }
    }
    loop (as)
  }


  def msort[T]( xs: List[T])(implicit less: (T, T) => Boolean): List[T] = {



    def accsort (acc: List[T], pending: List[T]): List[T] ={


      pending match {
        case h::Nil => acc
        case h::t if less(h, t.head) && !isSorted(t.tail) => accsort(acc:::List(h):::List(t.head), t.tail)
        case h::t if less(h, t.head) && isSorted(t.tail) => acc:::pending
        case h::t if !less(h, t.head) => accsort(acc:::List(t.head):::List(h), t.tail)
        case _ => acc

      }

    }
    accsort(List(), xs)

  }


  msort(List(2, 1))((a:Int,b:Int) => a<=b)


}
