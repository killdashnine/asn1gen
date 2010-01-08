package test.asn1.genruntime

import org.asn1gen.runtime.codec._

import scala.util.parsing.combinator.Parsers

trait ExemplarRealiser extends PackratBerRealiser with Parsers {
  type MySequence = (Option[Long], Double, AsnPrintableString, MyChoice)
  type MyChoice = test.asn1.genruntime.MyChoice
  
  def mkMySequence(value: Option[Long] ~ Double ~ AsnPrintableString ~ MyChoice): MySequence = {
    value match { case a ~ b ~ c ~ d => (a, b, c, d) }
  }

  def mkMyChoice_choice0(data: AsnNull): MyChoice = {
    test.asn1.genruntime.MyChoice_Choice0(data)
  }

  def mkMyChoice_choice1(data: AsnInteger): MyChoice = {
    test.asn1.genruntime.MyChoice_Choice1(org.asn1gen.runtime.AsnInteger(data))
  }
  
  def mkMyChoice_choice2(data: AsnReal): MyChoice = {
    test.asn1.genruntime.MyChoice_Choice2(org.asn1gen.runtime.AsnReal(data))
  }
}
