package org.asn1gen.parsing.asn1.ast

trait WithSyntaxSpec {
}

case class Word(
  chars: String
) extends Node {
  def name = chars
}

