package org.asn1gen.parsing.asn1.ast

case class TypeFieldReference(chars : String) extends Node {
  def name = chars
}

