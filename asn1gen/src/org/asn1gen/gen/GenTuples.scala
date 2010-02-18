package org.asn1gen.gen

object GenTuples {
  def main(args : Array[String]) : Unit = {
    val start: Int = java.lang.Integer.parseInt(args(0))
    val end: Int = java.lang.Integer.parseInt(args(1))
    
    for (i <- start to end) {
      println(i)
    }
  }
}
