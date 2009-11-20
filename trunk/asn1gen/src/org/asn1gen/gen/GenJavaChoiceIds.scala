package org.asn1gen.gen

import java.io.PrintWriter
import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.io._

class GenJavaChoiceIds(out: IndentWriter) {
  
  def generate(moduleDefinition: ModuleDefinition): Unit = {
    moduleDefinition match {
      case moduleDefinition@ModuleDefinition(
        ModuleIdentifier(
          ModuleReference(moduleName),
          DefinitiveIdentifier(_)),
        TagDefault(),
        ExtensionDefault(_),
        ModuleBody(_, _, assignmentList))
      => {
        out.println("package " + moduleName +";")
        out.println()
        generate(assignmentList)
        out.println()
      }
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
          Type_(
            BuiltinType(
              ChoiceType(
                AlternativeTypeLists(rootAlternativeTypeList, _, _, _))),
            _)))
      => {
        out.println("public class " + name + " {")
        out.indent(2) {
          generate(rootAlternativeTypeList)
        }
        out.println("}")
      }
    }
  }
  
  def generate(rootAlternativeTypeList: RootAlternativeTypeList): Unit = {
    rootAlternativeTypeList match {
      case RootAlternativeTypeList(AlternativeTypeList(namedTypes)) => {
        namedTypes foreach { namedType =>
          generate(namedType)
        }
      }
    }
  }
  
  def generate(namedType: NamedType): Unit = {
    println("XX: " + namedType)
    namedType match {
      case NamedType(
        Identifier(name),
        Type_(
          BuiltinType(
            TaggedType(
              Tag(_, ClassNumber(Number(tagNumber))),
              _, _)),
          _))
      => {
        out.println("public static int " + name + " = " + tagNumber + ";")
      }
    }
  }
}
