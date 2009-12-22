package test.asn1.genruntime

import org.asn1gen.runtime.codec._

object TestCodec {
  def main(args : Array[String]) : Unit = {
    val x = new OctetWindow(Array[Byte](1, 2, 3), 0, 3)
    println("Hello " + x(0) + " " + x(1) + " " + x(2))
  }
}
