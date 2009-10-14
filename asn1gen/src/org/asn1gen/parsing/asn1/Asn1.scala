package org.asn1gen.parsing.asn1

object Asn1 extends Parser {
  def parse(input: String): Option[Any] =
    phrase(Root)(new lexical.Scanner(input)) match {
      case Success(result, x) => { println(x); Some(result) }
      case x => { println(x); None }
    }
}
