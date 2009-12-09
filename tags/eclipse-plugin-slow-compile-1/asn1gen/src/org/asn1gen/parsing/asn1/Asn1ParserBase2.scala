package org.asn1gen.parsing.asn1

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

import org.asn1gen.parsing.syntax._

abstract class Asn1ParserBase2 extends TokenParsers {
  def elem[U](kind: String)(f: PartialFunction[Elem, U]): Parser[U] =
    elem(kind, {_: Elem => true}) ^? (f, _ => "Expecting " + kind + ".")

  def op(chars: String): Parser[Unit] = elem("operator " + chars) {
    case _ => ()
  }
}
