package com.github.scouto.sesion10

/**
  * Created by scouto.
  */
object Sesion10 extends App{


  def duplicateStrict(x: Double): Double = {
    println("hello")
    val result = x*2
    println(result)
    result
  }

  def duplicateNonStrict(x: => Double): Double = {
    println("hello")
    val result = x*2
    println(result)
    result
  }

  //duplicateNonStrict(10+11)
  //duplicateNonStrict(sys.error("failure"))

  def myIf[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
      if (cond) onTrue else onFalse
  }

  def myIf2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = {
    if (cond) onTrue() else onFalse()
  }

  def maybeDuplicate(cond: Boolean, elem: => Int): Int = {
    if (cond) elem + elem else 0
  }

  def maybeDuplicate2(cond: Boolean, elem: => Int): Int = {
    lazy val j = elem
    if (cond) j + j else 0
  }

  maybeDuplicate(true, {println("hi"); 1+41})
  maybeDuplicate2(true, {println("hi"); 1+41})

}
