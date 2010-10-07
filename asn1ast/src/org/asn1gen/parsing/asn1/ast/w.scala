package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

trait WithSyntaxSpec {
}

case class Word(
  chars: String
) extends Node {
  def name = chars
}

