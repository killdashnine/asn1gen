package org.asn1gen.parsing.asn1.ast

case class SimpleTableConstraint(
  objectSet: ObjectSet
) extends Node with TableConstraint {
}
