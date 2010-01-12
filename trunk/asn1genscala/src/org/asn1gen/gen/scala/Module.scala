package org.asn1gen.gen.scala

import org.asn1gen.parsing.asn1.{ast => ast}
import scala.collection.immutable._

case class Module(name: String, types: HashMap[String, NamedType], values: HashMap[String, NamedValue]) {
}

object Module {
  def from(moduleDefinition: ast.ModuleDefinition): Module = {
    val astAssignments = moduleDefinition.moduleBody.assignmentList.assignments
    val types = HashMap(astAssignments.partialMap { case t: ast.TypeAssignment =>
      (t.name -> NamedType.from(t))
    }: _*)
    val values = (HashMap[String, NamedValue]() /: astAssignments) {
      case (values, va: ast.ValueAssignment) =>
        values + (va.name -> NamedValue(va.name))
      case (values, _) => values
    }
    Module(moduleDefinition.name, types, values)
  }
}
