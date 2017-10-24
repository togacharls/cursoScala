package com.github.scouto.sesion4

/**
  * Created by scouto.
  */
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by scouto.
  */
class Sesion4Test extends FlatSpec with Matchers{
  val manjarin = Alumno("Javier", "Manjarin")
  val claudio  = Alumno(nombre = "Claudio", apellidos = "Barragan")
  val alfredo   = Alumno(apellidos = "Santaelena", nombre = "Alfredo")
  val fran  = Alumno("Fran", "González")

  val asignatura = Asignatura(
    nombre = "curso scala",
    descripcion = Some("Curso impartido en Amaris"),
    plazas = 3)

  "Administracion" should "permitir inscribirse si hay plazas" in {
    val adm = Administracion(Map(asignatura -> List(manjarin, claudio)))
    val optAdm = adm.alta(alfredo, asignatura)

    {adm.alta(alfredo, asignatura).get match {
      case Administracion(m) if m == Map(asignatura -> List(alfredo, manjarin, claudio)) => true
      case _ => false
    }} should be (true)

  }

  it should "rechazar la inscipcion si ya está inscrito" in {
    val adm = Administracion(Map(asignatura -> List(manjarin, claudio)))

    adm.alta(manjarin, asignatura) should be (None)
  }

  it should "rechazar la inscipcion si no quedan plazas" in {
    val adm = Administracion(Map(asignatura -> List(manjarin, claudio, alfredo)))

    adm.alta(fran, asignatura) should be (None)
  }

  it should "levantar un mensaje de error si el alumno no estaba inscrito" in {
    val adm = Administracion(Map(asignatura -> List(manjarin, claudio, alfredo)))
    val result = adm.baja(fran, asignatura)

    result should be (Left("Alumno no inscrito"))
  }

  it should "permitir si el alumno esta presente" in {
    val adm = Administracion(Map(asignatura -> List(manjarin, claudio, alfredo)))
    val result = adm.baja(manjarin, asignatura)

    result.right.get.relacionAlumnos(asignatura).size should be (2)
    result.right.get.relacionAlumnos should be (Map(asignatura -> List(claudio, alfredo)))
  }
}

