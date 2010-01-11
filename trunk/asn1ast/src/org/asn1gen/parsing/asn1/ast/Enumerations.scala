package org.asn1gen.parsing.asn1.ast

case class Enumerations(
  rootEnumeration: RootEnumeration,
  extension: Option[EnumerationsExtension1]
) extends Node {
}
