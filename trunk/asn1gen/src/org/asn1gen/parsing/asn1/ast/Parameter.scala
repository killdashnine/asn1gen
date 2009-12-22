package org.asn1gen.parsing.asn1.ast

case class Parameter(
    paramGovernor: Option[ParamGovernor],
    dummyReference: DummyReference
) extends Node {
}

