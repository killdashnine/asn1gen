package org.asn1gen.parsing.asn1.ast

case class ExtensionAdditionAlternativesList(
  extensionAdditionAlternatives: List[ExtensionAdditionAlternative]
) extends Node with ExtensionAdditionAlternatives {
}
