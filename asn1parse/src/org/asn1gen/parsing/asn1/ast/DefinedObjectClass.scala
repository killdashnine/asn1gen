package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

trait DefinedObjectClass
  extends GovernorKind
  with ObjectClass
  with ActualParameter
  with UserDefinedConstraintParameter {
}
