package org.asn1gen.gen

import java.io.PrintWriter
import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.io._

class GenJava(out: IndentWriter) {
  
  def generate(moduleDefinition: ModuleDefinition): Unit = {
    moduleDefinition match {
      case moduleDefinition@ModuleDefinition(
        ModuleIdentifier(
          ModuleReference(moduleName),
          DefinitiveIdentifier()),
        TagDefault(),
        ExtensionDefault(),
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
          Type(
            BuiltinType(
              ChoiceType(
                AlternativeTypeLists(rootAlternativeTypeList, _, _, _))))))
      => {
        out.println("public class " + name + " {")
        out.indent(2) {
          out.println("//////////////////////////////////////////////////////////////////")
          out.println("// Choice IDs")
          generateChoiceIds(rootAlternativeTypeList)
          out.println("private AsnType elem_;")
          out.println()
          generateSimpleGetters(rootAlternativeTypeList)
        }
        out.println("}")
      }
    }
  }
  
  def generateChoiceIds(rootAlternativeTypeList: RootAlternativeTypeList): Unit = {
    rootAlternativeTypeList match {
      case RootAlternativeTypeList(namedTypes) => namedTypes foreach { namedType =>
        generateChoiceIds(namedType)
      }
    }
  }
  
  def generateChoiceIds(namedType: NamedType): Unit = {
    namedType match {
      case NamedType(
        Identifier(name),
        Type(
          BuiltinType(
            DefaultTaggedType(
              Tag(_, LiteralClassNumber(Number(tagNumber))),
              _))))
      => {
        out.println()
        out.println("@ChoiceId")
        out.println("public static int " + name.toUpperCase + " = " + tagNumber + ";")
      }
    }
  }

  def generateSimpleGetters(rootAlternativeTypeList: RootAlternativeTypeList): Unit = {
    rootAlternativeTypeList match {
      case RootAlternativeTypeList(namedTypes) => namedTypes foreach { namedType =>
        generateSimpleGetters(namedType)
      }
    }
  }
  
  def generateSimpleGetters(namedType: NamedType): Unit = {
    namedType match {
      case NamedType(
        Identifier(name),
        Type(
          BuiltinType(
            DefaultTaggedType(
              _,
              type_))))
      => {
        val getter = "get" + name.first.toUpperCase + name.substring(1)
        out.println()
        out.println("public AsnInteger " + getter + "() {")
        out.indent(2) {
          out.println("return (AsnInteger)elem_;")
        }
        out.println("}")
      }
    }
  }
}
