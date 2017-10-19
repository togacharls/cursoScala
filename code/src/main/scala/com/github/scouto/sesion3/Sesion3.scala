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

  def uncurry(f: Int => Int => Int): (Int, Int) => Int = ???


  def curry(f: (Int, Int) => Int): Int => Int => Int = ???


}

class Person (private val _name: String, private var _age: Int = 0) {

 def name = _name

 def age = _age

  def age_(newAge: Int) = _age = newAge

}

