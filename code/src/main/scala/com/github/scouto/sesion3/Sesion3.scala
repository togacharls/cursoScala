package com.github.scouto.sesion3

import java.sql.{Date => SqlDate}
import java.util.{Date => UtilDate}


/**
  * Created by scouto.
  */
object Sesion3 extends App{

  val date = new SqlDate(System.currentTimeMillis())
  println(date.getClass)

  val dateU = new UtilDate(System.currentTimeMillis())
  println(date.getClass)

  def uncurry(f: Int => Int => Int): (Int, Int) => Int = (a, b) => f(a)(b)

  def curry(f: (Int, Int) => Int): Int => Int => Int = a => b => f(a, b)

  def composicion(f:Int => String, g:Int => Int): Int => String = a => f(g(a))
  /*f compose g*/


  /*Forma genérica. Es decir, es independiente al tipo de los parámetros*/
  def uncurryGen[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def curryGen[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def composicionGen[A, B, C](f:B => C, g:A => C): A => C = a => f(g(a))
}

class Person (private val _name: String, private var _age: Int = 0) {

 def name = _name

 def age = _age

  def age_(newAge: Int) = _age = newAge

}

