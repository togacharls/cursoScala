package com.github.scouto

/**
  * Created by scouto.
  */
object MyApp extends App {
  println("Hello World!")

  def sum(x: Int, y: Int): Int = {
    x + y
  }

  def wrongFactorial(n: Int): Int = {
    if (n <= 0) n
    else n * factorial(n - 1)
  }


  def factorial(n: Int): Int = {

    @annotation.tailrec
    def rec(acc: Int, current: Int): Int = {
      if (current <= 0) acc
      else rec (acc*current, current -1)

    }

    rec (1, n)
  }


}








