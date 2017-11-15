package com.github.scouto.sesion9

/**
  * Created by scouto.
  */
object Weekday extends Enumeration {

  //val Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo = Value

  val Monday = Value("Lunes")
  val Tuesday = Value("Martes")
  val Wednesday = Value("Miercoles")
  val Thursday = Value("Jueves")
  val Friday = Value("Viernes")
  val Saturday = Value("Sabado")
  val Sunday = Value("Domingo")

}

object MyApp extends App{

  def laborable(weekday: Weekday.Value): Boolean = {
    weekday match {
      case Weekday.Sunday => false
      case Weekday.Monday => true
      case Weekday.Tuesday => true
      case Weekday.Wednesday => true
      case Weekday.Thursday => true
      case Weekday.Friday => true
      case Weekday.Saturday => true
    }
  }
  println(Weekday.values.toList.sortBy(_.toString))
  //println(Weekday.values.toList.sortBy(_.id)) == println(Weekday.values.toList.sorted())

}




