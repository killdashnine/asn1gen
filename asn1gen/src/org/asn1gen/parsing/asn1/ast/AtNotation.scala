package org.asn1gen.parsing.asn1.ast

case class AtNotation(
  componentIdList: ComponentIdList,
  hasDot: Boolean
) extends Node {
}
