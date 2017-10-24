package com.github.scouto.sesion4

/**
  * Created by scouto.
  */
class Asignatura (val nombre: String, val plazas: Int = 30, val descripcion: Option[String] = None) {

}

object Asignatura {
  def apply(nombre: String, plazas: Int, descripcion: Option[String] ) = new Asignatura(nombre, plazas, descripcion)
  def unapply(asig: Asignatura ): Option[(String, Int, Option[String])] = {
    Some(asig.nombre, asig.plazas, asig.descripcion)
  }
}
