package test.asn1.genruntime

import org.asn1gen.runtime._

object ScalaCodeUser {
  def main(args : Array[String]) : Unit = {
    val mySequence = MySequence(
        Some(AsnInteger(1)),
        AsnReal(1.0),
        AsnPrintableString("Hello world"),
        MyChoice.Choice1(AsnInteger(2)))
    val mySequence2 =
      ( mySequence
          .field0{
            case Some(AsnInteger(x)) => Some(AsnInteger(x + 2))
            case None => None
          }
          .field1{_ => AsnReal(3.0)}
      )
    println(mySequence2)
    println(mySequence2.field3._choice)
    println(mySequence.field3{_.choice2{_ => AsnReal(9.9)}})
    println(AsnNull)
    println(MySequence)
    println(MyChoice)
    val moo =
      ( MySequence
          .field0{_ => Some(AsnInteger(12))}
          .field1{_ => AsnReal(123.0)}
      )
    println(moo)
    println(moo.field3.choice2{_ => AsnReal(1.23)}._choice)
    println(MyEnum.value1)
    //println(MySequence)
  }
}
