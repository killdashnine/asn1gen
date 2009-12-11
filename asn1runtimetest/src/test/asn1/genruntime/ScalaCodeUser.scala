package test.asn1.genruntime

import org.asn1gen.runtime._

object ScalaCodeUser {
  def main(args : Array[String]) : Unit = {
    val mySequence = MySequence(
        AsnInteger(1),
        AsnReal(1.0),
        AsnPrintableString("Hello world"),
        MyChoice(AsnInteger(1)))
    val mySequence2 =
      ( mySequence
          .field1{case AsnInteger(x) => AsnInteger(x + 2)}
          .field2{_ => AsnReal(3.0)}
      )
    println(mySequence2)
    println(MySequence)
  }
}
