package com.github.scouto.sesion9

/**
  * Created by scouto.
  */

object WeekdayBasic extends Enumeration {
  val Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo = Value
}

object Weekday extends Enumeration {

//  val Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo = Value

  val Monday = Value("Lunes")
  val Tuesday = Value("Martes")
  val Wednesday = Value("Miercoles")
  val Thursday = Value("Jueves")
  val Friday = Value("Viernes")
  val Saturday = Value("Sabado")
  val Sunday = Value("Domingo")

}

object WeekdayOwn extends Enumeration {

  //  val Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo = Value

  case class WeekdayValue(i: Int, name: String, laborable: Boolean) extends Val(i: Int, name: String) {
    def isLaborable = laborable
  }


  val Monday = WeekdayValue(0, "Lunes", true)
  val Tuesday = WeekdayValue(1, "Martes", true)
  val Wednesday = WeekdayValue(2, "Miercoles", true)
  val Thursday = WeekdayValue(3, "Jueves", true)
  val Friday = WeekdayValue(4, "Viernes", true)
  val Saturday = WeekdayValue(5, "Sabado", false)
  val Sunday = WeekdayValue(6, "Domingo", false)


}

object MyApp extends App{

  def isLaborable(weekday: Weekday.Value): Boolean = {
      weekday match {
        case Weekday.Sunday => true
        case Weekday.Saturday => true
        case _ => true
      }
  }

  def isLaborableOwn(weekday: WeekdayOwn.WeekdayValue): Boolean = {
    weekday.isLaborable
  }

  val weekDayValuesList = Weekday.values.toList
  val weekDayValuesMap = weekDayValuesList.map(elem => (elem.id, elem.toString)).toMap

  println(
    weekDayValuesList.sortBy(_.toString)
  )

  println(
    weekDayValuesMap.toList.sorted
  )


}




