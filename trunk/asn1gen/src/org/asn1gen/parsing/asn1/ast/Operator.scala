package org.asn1gen.parsing.asn1.ast

case class Operator(chars : String) extends Node {
  def name = chars
}

