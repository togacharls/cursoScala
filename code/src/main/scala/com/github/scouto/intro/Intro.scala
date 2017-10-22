package com.github.scouto.intro

/**
  * Created by scouto.
  */
object MyApp extends App {
  println("Hello World ")

  def sum(x: Int, y: Int): Int = {
    x + y
  }

  def wrongFactorial(n: Int): Int = {
    if (n <= 0) 1
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

  /*
  * Define una función recursiva que devuelva el máximo de una lista de enteros
  * */
  def max(list: List[Int]) : Int = {
    list.length match {
      case 0 => throw new Exception("Empty list")
      /*list.head == list(0)*/
      case 1 => list.head
      /*list.tail == list.drop(1)*/
      case _ => {
        val maxTail = max(list.tail)
        if(list(0) > maxTail) list(0)
        else maxTail
      }
    }
  }

  /*
  * Función que devuelve el segundo elemento de una lista
  * */
  def second(list: List[Int]): Int = list.tail.head

  /*
  * Función que devuelva el nth elemento de una lista
  * */
  def nth(list: List[Int], n: Int): Int = {
    if(n > list.length || n < 0) throw new Exception(s"list(${n}) doesn't exists")
    if(n == 0) list.head
    else nth(list.tail, n-1)
  }


}








