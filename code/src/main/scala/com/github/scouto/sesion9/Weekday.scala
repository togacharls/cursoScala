package com.github.scouto.sesion9

/**
  * Created by scouto.
  */
object Weekday extends Enumeration {

  val Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo = Value

  case class WeekdayValue(val i: Int, val name: String, val laborable: Boolean, val horasQueTRabajo: Int) extends Val(i: Int, name: String) {
    def isLaborable = laborable
  }


  val Monday = WeekdayValue(1, "Lunes", true, 8)
  val Tuesday = Value("Martes")
  val Wednesday = Value("Miercoles")
  val Thursday = Value("Jueves")
  val Friday = Value("Viernes")
  val Saturday = Value("Sabado")
  val Sunday = Value("Domingo")

}

object MyApp extends App{

  println(
    Weekday.values.toList
  )


}




