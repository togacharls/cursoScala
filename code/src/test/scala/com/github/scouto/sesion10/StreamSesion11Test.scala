package com.github.scouto.sesion10

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

/**
  * Created by couto on 30/06/17.
  */
class StreamSesion11Test extends FlatSpec with Matchers with PropertyChecks {

  val stream = Stream(1,2,3)
  val streamString = Stream("1","2","3")
  val size1Stream = Stream("Hello")
  val emptyStream = Stream()
  val emptyIntStream: Stream[Int] = Stream()

  "existsFoldRight" should "work as exists" in {
    emptyIntStream.existsFoldRight(_ > 5) should be (emptyIntStream.exists(_ > 5))
    stream.existsFoldRight(_ == 1) should be (stream.exists(_ == 1))
    stream.existsFoldRight(_ == 2) should be (stream.exists(_ == 2))
    stream.existsFoldRight(_ == 3) should be (stream.exists(_ == 3))
    stream.existsFoldRight(_ == 4) should be (stream.exists(_ == 4))
    stream.existsFoldRight(_ < 0) should be (stream.exists(_ < 0))
  }

  "existsFoldLeft" should "work as exists" in {
    emptyIntStream.existsFoldLeft(_ > 5) should be (emptyIntStream.exists(_ > 5))
    stream.existsFoldLeft(_ == 1) should be (stream.exists(_ == 1))
    stream.existsFoldLeft(_ == 2) should be (stream.exists(_ == 2))
    stream.existsFoldLeft(_ == 3) should be (stream.exists(_ == 3))
    stream.existsFoldLeft(_ == 4) should be (stream.exists(_ == 4))
    stream.existsFoldLeft(_ < 0) should be (stream.exists(_ < 0))
  }

  "forAll" should "return true if empty Stream" in {
    emptyIntStream.forAll(_ > 5) should be (true)
  }

  it should "return true if all the elements in the Stream satisfies the predicate" in {
    stream.forAll(_ > 0) should be (true)
    streamString.forAll(x => Try(x.toInt).isSuccess) should be (true)
  }
  
  it should "return false if at least one elements in the Stream does not satisfy the predicate" in {
    stream.forAll(_ > 1) should be (false)
    size1Stream.forAll(x => Try(x.toInt).isSuccess) should be (false)
  }



}
