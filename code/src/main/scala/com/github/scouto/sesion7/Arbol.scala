package com.github.scouto.sesion7

import scala.annotation.tailrec

/**
  * Created by couto on 27/06/17.
  */
sealed trait Arbol[+A]
case class Hoja[A](value: A) extends Arbol[A]
case class Rama[A](left: Arbol[A], right: Arbol[A]) extends Arbol[A]


object Arbol {

  def size[A] (t: Arbol[A]): Int = {
    t match {
      case Hoja(_) => 1
      case Rama(l, r) => size(l) + size(r) + 1
    }
  }

  def maximum (t: Arbol[Int]): Int = {
    t match {
      case Hoja(_) => _
      case Rama(l, r) => {
        val mL = maximum(l)
        val mR = maximum(r)
        mL max mR
      }
    }
  }

  def depth[A] (t:Arbol[A]) : Int = {
    t match {
      case Hoja(_) => 1
      case Rama(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A, B] (t:Arbol[A])(f:A => B): Arbol[B] = {
    t match {
      case Hoja(_) => Hoja(f(t))
      case Rama(l, r) => Rama(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B] (t:Arbol[A])(f: A=> B)(g: (B, B) => B): B = {
    t match {
      case Hoja(a) => f(a)
      case Rama(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sizeFold[A] (t: Arbol[A]): Int = {
    fold(t)(a => 1) ((l, r) => l + r + 1)
  }

  def maximumFold (t: Arbol[Int]): Int = {
    fold(t)(a => a) ((l, r) => l max r)
  }

  def depthFold[A] (t:Arbol[A]) : Int = {
    fold(t)(a=>1) ((l, r) => 1 + (l max r))
  }

  def mapFold[A, B] (t:Arbol[A])(f:A => B): Arbol[B] = {
    fold(t)(a=> Hoja(f(a)): Arbol[B]) ((l, r) => Rama(l, r))
  }
}