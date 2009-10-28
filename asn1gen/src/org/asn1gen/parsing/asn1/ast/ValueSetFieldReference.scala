package org.asn1gen.parsing.asn1.ast

case class ValueSetFieldReference(chars : String) extends Node {
  def name = chars
}

