package org.asn1gen.parsing.asn1.ast

case class IntersectionElements(
    elements: Elements,
    exclusions: Option[Exclusions]
) extends Node {
}
