package com.github.scouto.sesion4.traits

/**
  * Created by scouto.
  */
sealed trait AsignaturaTrait{
  val nombre: String
  val plazas: Int = 30
  val descripcion: Option[String] = None
}
case class AsignaturaConPrioridad(nombre: String, override val plazas: Int, override val descripcion: Option[String]) extends AsignaturaTrait
case class AsignaturaSinPrioridad(nombre: String, override val plazas: Int, override val descripcion: Option[String]) extends AsignaturaTrait


