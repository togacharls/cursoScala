package com.github.scouto.sesion4

/**
  * Created by scouto.
  */
class Administracion(val relacionAlumnos: Map[Asignatura, List[Alumno]] = Map()) {

  /**
    * Debe dar de alta un alumno si no supera el máximo y el alumno no está ya presente
    * @param alumno
    * @param asignatura
    * @return
    */
  def alta(alumno: Alumno, asignatura: Asignatura): Option[Administracion] = {
    val alumnos = relacionAlumnos.getOrElse(asignatura, List())
    alumnos match {
      case Nil => Some(Administracion(relacionAlumnos + (asignatura -> List(alumno))))
      case l if l.contains(alumno) => None
      case l if l.size < asignatura.plazas => Some(Administracion(relacionAlumnos + (asignatura -> (alumno :: l))))
      case _ => None
    }

//    val alumnos = relacionAlumnos.getOrElse(asignatura, List())
//    if ((alumnos.size >= asignatura.plazas) || alumnos.contains(alumno))
//      None
//    else
//      Some(new Administracion(relacionAlumnos + (asignatura -> (alumno :: alumnos))))
  }

  /**
    * Debe dar de baja un alumno o levantar un error si no es posible
    * @param alumno
    * @param asignatura
    * @return
    */
  def baja(alumno: Alumno, asignatura: Asignatura): Either[String, Administracion] = {

    val alumnos = relacionAlumnos.getOrElse(asignatura, List())

    alumnos match {
      case Nil => Left("Alumno no inscrito")
      case l if l.contains(alumno) => Right(Administracion(relacionAlumnos + (asignatura -> alumnos.filterNot(_ == alumno))))
      case _ => Left("Alumno no inscrito")
    }


    //    if (alumnos.contains(alumno))
    //      Right(new Administracion(relacionAlumnos + (asignatura -> (alumnos.filterNot(_ ==alumno)))))
    //    else
    //      Left("Alumno no inscrito")

  }
}

object Administracion {
  def apply(relacionAlumnos: Map[Asignatura, List[Alumno]])  = new Administracion(relacionAlumnos)

  def unapply(adm: Administracion): Option[Map[Asignatura, List[Alumno]]] = {
    Some(adm.relacionAlumnos)
  }


}
