package org.asn1gen.parsing.asn1

object Asn1 extends Parser {
  def parse(input: String) = phrase(root)(new lexical.Scanner(input))
}
