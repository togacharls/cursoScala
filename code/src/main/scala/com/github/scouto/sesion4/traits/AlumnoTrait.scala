package com.github.scouto.sesion4.traits

/**
  * Created by scouto.
  */
abstract class AlumnoTrait {
  val nombre :String
  val apellidos :String


  def isRepetidor: Boolean = ???
}
case class AlumnoNuevo(nombre:String, apellidos: String) extends AlumnoTrait {
  override def isRepetidor: Boolean = false

}
case class AlumnoRepetidor(nombre:String, apellidos: String) extends AlumnoTrait {
  override def isRepetidor: Boolean = true
}

