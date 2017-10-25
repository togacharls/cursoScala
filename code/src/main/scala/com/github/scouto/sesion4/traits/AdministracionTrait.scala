package com.github.scouto.sesion4.traits

/**
  * Created by scouto.
  */
case class AdministracionTrait[As <: AsignaturaTrait, Al <: AlumnoTrait](relacionAlumnos: Map[As, List[Al]] = Map()) {


  def removeFirstElement(list: List[Al], f: AlumnoTrait => Boolean): List[Al] = {

    @annotation.tailrec
    def go(acc: List[Al], rest: List[Al]): List[Al] = {

      rest match {
        case Nil => acc
        case h::t if f(h) => acc:::t
        case h::t if !f(h) => go(acc :+ h, t)
      }
    }

    go (List(), list)
  }

  /**
    * Debe dar de alta un alumno si no supera el máximo y el alumno no está ya presente
    * @param alumno
    * @param asignatura
    * @return
    */
  def alta(alumno: Al, asignatura: As): Option[AdministracionTrait[As, Al]] = {

    def altaConPrioridad(alumno: Al, asignatura: As):  Option[AdministracionTrait[As, Al]] = {

        relacionAlumnos.getOrElse(asignatura, List()) match {
        case Nil => Some(AdministracionTrait(relacionAlumnos + (asignatura -> List(alumno))))
        case l if l.contains(alumno) => None
        case l if l.size < asignatura.plazas
                => Some(AdministracionTrait(relacionAlumnos + (asignatura -> (alumno :: l))))
        case l if l.size == asignatura.plazas
          && l.exists(_.isRepetidor)
                => Some(AdministracionTrait(relacionAlumnos + (asignatura -> (alumno :: removeFirstElement(l, a => a.isRepetidor)))))

        case _ => None
      }
    }


    def altaSinPrioridad(alumno: Al, asignatura: As): Option[AdministracionTrait[As, Al]] = {
        relacionAlumnos.getOrElse(asignatura, List()) match {
        case Nil => Some(AdministracionTrait(relacionAlumnos + (asignatura -> List(alumno))))
        case l if l.contains(alumno) => None
        case l if l.size < asignatura.plazas => Some(AdministracionTrait(relacionAlumnos + (asignatura -> (alumno :: l))))
        case _ => None
      }
    }


    (asignatura, alumno) match {
      case (AsignaturaConPrioridad(_, _, _), AlumnoNuevo(_, _)) => altaConPrioridad(alumno, asignatura)
      case _ => altaSinPrioridad(alumno, asignatura)
    }

  }

  /**
    * Debe dar de baja un alumno o levantar un error si no es posible
    * @param alumno
    * @param asignatura
    * @return
    */
  def baja(alumno: Al, asignatura: As): Either[String, AdministracionTrait[As, Al]] = {

    val alumnos = relacionAlumnos.getOrElse(asignatura, List())

    alumnos match {
      case Nil => Left("Alumno no inscrito")
      case l if l.contains(alumno) => Right(AdministracionTrait(relacionAlumnos + (asignatura -> alumnos.filterNot(_ == alumno))))
      case _ => Left("Alumno no inscrito")
    }

  }
}


