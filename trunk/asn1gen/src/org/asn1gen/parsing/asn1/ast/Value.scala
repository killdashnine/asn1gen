package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

trait Value extends ActualParameterKind
  with GovernorConstraintParameterValue
  with LowerEndValueKind
  with SettingKind
  with UpperEndValueKind {
}
