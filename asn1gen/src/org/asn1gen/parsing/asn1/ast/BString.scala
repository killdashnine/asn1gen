package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class BString(
  chars: String
) extends Node
  with OctetStringValueKind
  with BitStringValueKind
{
}
