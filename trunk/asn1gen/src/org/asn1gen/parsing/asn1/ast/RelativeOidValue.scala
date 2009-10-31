package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class RelativeOidValue(
  relativeOidComponentsList: List[RelativeOidComponents]
) extends Node with BuiltinValueKind {
}
