package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

object MINUS_INFINITY extends SpecialRealValue {
}

object Max extends UpperEndValue {
}

object Min extends LowerEndValue {
}

case class ModuleBody(exports: Exports, imports: Imports, assignmentList: AssignmentList) extends Node {
  def this() = {this(Exports(None), Imports(None), AssignmentList(Nil))}
}


case class ModuleDefinition(
  identifier: ModuleIdentifier,
  tagDefault: TagDefault,
  extensionDefault: ExtensionDefault,
  moduleBody: ModuleBody
) extends Node {
  def name = identifier.name
  def exports = moduleBody.exports
  def imports = moduleBody.imports
  def assignmentList = moduleBody.assignmentList
}

case class ModuleIdentifier(
  reference: ModuleReference,
  definitiveIdentifier: DefinitiveIdentifier
) extends Node {
  def name = reference.name
}


case class ModuleReference(
  name: String
) extends Node {
}

trait MultipleTypeConstraints extends InnerTypeConstraints {
}
