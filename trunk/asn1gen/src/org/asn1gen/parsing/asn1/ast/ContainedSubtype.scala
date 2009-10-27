package org.asn1gen.parsing.asn1.ast

case class ContainedSubtype(
  includes: Includes,
  type_ : Type_
) extends Node {
}

