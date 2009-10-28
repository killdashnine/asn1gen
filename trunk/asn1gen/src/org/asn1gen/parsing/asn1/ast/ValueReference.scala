package org.asn1gen.parsing.asn1.ast

case class ValueReference(chars : String) extends Node {
  def name = chars
}

