package org.asn1gen.gen.scala

import org.asn1gen.parsing.asn1.{ast => ast}
import scala.collection.immutable._

case class Module(name: String, types: HashSet[NamedType], values: HashSet[NamedValue]) {
}

object Module {
  def from(moduleDefinition: ast.ModuleDefinition): Module = {
    val astAssignments = moduleDefinition.moduleBody.assignmentList.assignments
    val types = astAssignments.foldLeft(HashSet[NamedType]()) { (types, astAssignment) =>
      astAssignment match {
        case ta@ast.TypeAssignment(typeReference, _type) => {
          types + NamedType(typeReference.name)
        }
        case _ => types
      }
    }
    val values = astAssignments.foldLeft(HashSet[NamedValue]()) { (values, astAssignment) =>
      astAssignment match {
        case ta@ast.ValueAssignment(valueReference, _type, value) => {
          values + NamedValue(valueReference.name)
        }
        case _ => values
      }
    }
    Module(moduleDefinition.name, types, values)
  }
}
