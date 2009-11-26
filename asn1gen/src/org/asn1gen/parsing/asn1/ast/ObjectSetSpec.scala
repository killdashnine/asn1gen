package org.asn1gen.parsing.asn1.ast

case class ObjectSetSpec(
    rootElementSetSpec: Option[RootElementSetSpec],
    additionalElementSetSpec: Option[AdditionalElementSetSpec]
) extends Node {
}

