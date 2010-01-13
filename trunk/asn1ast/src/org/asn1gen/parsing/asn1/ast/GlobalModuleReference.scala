package org.asn1gen.parsing.asn1.ast

case class GlobalModuleReference(
  moduleReference: ModuleReference,
  assignedIdentifier: AssignedIdentifier
) extends Node {
  def name = moduleReference.name
}
