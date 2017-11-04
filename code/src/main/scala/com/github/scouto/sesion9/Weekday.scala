package com.github.scouto.sesion9

/**
  * Created by scouto.
  */
object Weekday extends Enumeration {

  val Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo = Value

  class WeekdayValue(val i: Int, val name: String, val laborable: Boolean) extends Val(i: Int, name: String) {
    def isLaborable = laborable
  }


  val Monday = Value("Lunes")
  val Tuesday = Value("Martes")
  val Wednesday = Value("Miercoles")
  val Thursday = Value("Jueves")
  val Friday = Value("Viernes")
  val Saturday = Value("Sabado")
  val Sunday = Value("Domingo")

}

object MyApp extends App{

  def laborable(weekday: Weekday.Value): Boolean = ???


}




