package org.asn1gen.gen

import java.io.PrintWriter
import org.asn1gen.parsing.asn1.ast._

class GenJavaChoiceIds(out: PrintWriter) {
  
  def generate(moduleDefinition: ModuleDefinition): Unit = {
    moduleDefinition match {
      case moduleDefinition@ModuleDefinition(
        ModuleIdentifier(
          ModuleReference("ModuleName"),
          DefinitiveIdentifier()),
        TagDefault(),
        ExtensionDefault(),
        ModuleBody(_, _, assignmentList))
      => generate(assignmentList)
    }
  }

  def generate(assignmentList: AssignmentList): Unit = {
    assignmentList match {
      case AssignmentList(assignments) => assignments foreach { assignment =>
        generate(assignment)
      }
    }
  }

  def generate(assignment: Assignment): Unit = {
    assignment match {
      case Assignment(
        TypeAssignment(
          TypeReference(name),
          Type(
            BuiltinType(
              ChoiceType(
                AlternativeTypeLists(rootAlternativeTypeList, _, _, _))))))
      => {
        println(name)
        generate(rootAlternativeTypeList)
      }
    }
  }
  
  def generate(rootAlternativeTypeList: RootAlternativeTypeList): Unit = {
    rootAlternativeTypeList match {
      case RootAlternativeTypeList(namedTypes) => namedTypes foreach { namedType =>
        generate(namedType)
      }
    }
  }
  
  def generate(namedType: NamedType): Unit = {
    namedType match {
      case NamedType(
        Identifier(name),
        Type(
          BuiltinType(
            DefaultTaggedType(
              Tag(_, LiteralClassNumber(Number(tagNumber))),
              _))))
      => {
        println(name + " = " + tagNumber)
      }
    }
  }
}
