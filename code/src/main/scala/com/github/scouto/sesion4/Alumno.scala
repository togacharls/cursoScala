package com.github.scouto.sesion4

/**
  * Created by scouto.
  */



class Alumno(val nombre:String, val apellidos: String){


}


object Alumno {
  def apply(nombre: String, apellidos: String) = new Alumno(nombre, apellidos)
  def unapply(al: Alumno ): Option[(String, String)] = {
    Some(al.nombre, al.apellidos )
  }
}


