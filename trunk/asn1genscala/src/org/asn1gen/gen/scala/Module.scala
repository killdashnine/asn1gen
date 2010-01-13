package org.asn1gen.gen.scala

import org.asn1gen.parsing.asn1.{ast => ast}
import scala.collection.immutable._

case class Module(
    name: String,
    types: HashMap[String, NamedType],
    values: HashMap[String, NamedValue]) {
}

object Module {
  def from(moduleDefinition: ast.ModuleDefinition): Module = {
    val imports = moduleDefinition.imports
    val exports = moduleDefinition.exports
    println(imports)
    println(exports)
    val astAssignments = moduleDefinition.moduleBody.assignmentList.assignments
    val types = (HashMap[String, NamedType]() /: astAssignments) {
      case (types, ta: ast.TypeAssignment) =>
        val name = ta.name
        if (types contains name) throw new DuplicateTypeException(name)
        types + (name -> NamedType.from(ta))
      case (types, _) => types
    }
    val values = (HashMap[String, NamedValue]() /: astAssignments) {
      case (values, va: ast.ValueAssignment) =>
        val name = va.name
        if (values contains name) throw new DuplicateValueException(name)
        values + (name -> NamedValue.from(va))
      case (values, _) => values
    }
    Module(moduleDefinition.name, types, values)
  }
}
