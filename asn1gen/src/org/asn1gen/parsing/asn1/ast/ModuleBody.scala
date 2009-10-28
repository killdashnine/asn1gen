package org.asn1gen.parsing.asn1.ast

case class ModuleBody(exports : Exports, imports : Imports, assignmentList: AssignmentList) extends Node {
  def this() = {this(Exports(), Imports(), AssignmentList(Nil))}
}

