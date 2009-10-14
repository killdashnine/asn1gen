package org.asn1gen.parsing.asn1

object ParserTest {
  def main(args : Array[String]) : Unit = {
    def result = Asn1.parse("/*_modulereference_*/");
    println(result);
  }
}
