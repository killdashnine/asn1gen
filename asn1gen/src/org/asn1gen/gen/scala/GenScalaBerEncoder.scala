package org.asn1gen.gen.scala

import java.io.PrintWriter
import org.asn1gen.extra.Extras._
import org.asn1gen.io._
import org.asn1gen.parsing.asn1.{ast => ast}
import scala.collection.immutable.Set
import org.asn1gen.gen.scala.NameOf._

class GenScalaBerEncoder(packageName: String, out: IndentWriter) {
  val keywords = Set("yield", "type", "null", "final")
  
  def generate(module: Module): Unit = {
    ( out
      << "/* This file was generated by asn1gen */" << EndLn
      << EndLn
      << "package " << packageName << ".codec.ber" << EndLn
      << EndLn
      << "import org.asn1gen.{runtime => _rt_}" << EndLn
      << "import org.asn1gen.{io => _io_}" << EndLn
      << "import moo.{ASNEXAMPLES => _m_}" << EndLn
      << EndLn
      << "trait " << safeId(module.name) << " extends org.asn1gen.runtime.codec.BerEncoder {" << EndLn
      << EndLn
    )
    out.indent(2) {
      module.imports foreach { symbolsFromModule =>
        out << "import " << symbolsFromModule.module + "._" << EndLn
      }
      out << EndLn
      module.types.foreach { case (_, namedType: NamedType) =>
        generate(namedType)
      }
    }
    out << "}" << EndLn
  }
  
  def generate(namedType: NamedType): Unit = {
    namedType._type match {
      case ast.Type(builtinType: ast.BuiltinType, _) => {
        generate(builtinType, namedType.name)
      }
      case t@ast.Type(_, _) => {
        ( out
          << "/* unknown: " << namedType.name << EndLn
          << t << EndLn
          << "*/" << EndLn
        )
      }
    }
  }
  
  def generate(builtinType: ast.BuiltinType, assignmentName: String): Unit = {
    val safeAssignmentName = "_m_." + safeId(assignmentName)
    builtinType match {
      case ast.ChoiceType(
        ast.AlternativeTypeLists(rootAlternativeTypeList, _, _, _))
      => {
        out << "// Choice type" << EndLn
        out << "def encode("
        out << "value: " << safeAssignmentName
        out << "): _io_.ByteStreamer = {" << EndLn
        out.indent(2) {
          out << "_io_.ByteStreamer.nil" << EndLn
        }
        out << "}" << EndLn
        out << EndLn
        out << "def encodeData("
        out << "value: " << safeAssignmentName
        out << "): _io_.ByteStreamer = {" << EndLn
        out.indent(2) {
          rootAlternativeTypeList match {
            case ast.RootAlternativeTypeList(ast.AlternativeTypeList(namedTypes)) => {
              namedTypes foreach { namedType =>
                out << "// 1 -> " << namedType << EndLn
              }
            }
          }
          out << "_io_.ByteStreamer.nil" << EndLn
        }
        out << "}"
        out << EndLn
      }
      case ast.SequenceType(ast.Empty) => {
        out.ensureEmptyLines(1)
        out << "// Empty sequence type" << EndLn
        out << "def encode("
        out << "value: " << safeAssignmentName
        out << "): _io_.ByteStreamer = {" << EndLn
        out.indent(2) {
          out << "_io_.ByteStreamer.nil" << EndLn
        }
        out << "}" << EndLn
        out << EndLn
        out << "def encodeData("
        out << "value: " << safeAssignmentName
        out << "): _io_.ByteStreamer = {" << EndLn
        out.indent(2) {
          out << "// 2 -> Empty"
          out << "_io_.ByteStreamer.nil" << EndLn
        }
        out << "}" << EndLn
        out << EndLn
      }
      case ast.SequenceType(ast.ComponentTypeLists(list1, extension, list2)) => {
        val list = (list1.toList:::list2.toList).map { componentTypeList =>
          componentTypeList.componentTypes
        }.flatten
        out.ensureEmptyLines(1)
        out << "// Sequence type" << EndLn
        out << "def encode("
        out << "value: " << safeAssignmentName
        out << "): _io_.ByteStreamer = {" << EndLn
        out.indent(2) {
          out << "encodeSequence(encodeData(value))" << EndLn
        }
        out << "}" << EndLn
        out << EndLn
        out << "def encodeData("
        out << "value: " << safeAssignmentName
        out << "): _io_.ByteStreamer = {" << EndLn
        out.indent(2) {
          list foreach {
            case ast.NamedComponentType(ast.NamedType(ast.Identifier(identifier), _type), value) => {
              out << "// 3 -> " << identifier << EndLn
              out << "encode(value." << identifier << ")" << EndLn
            }
          }
          out << "_io_.ByteStreamer.nil" << EndLn
        }
        out << "}" << EndLn
        out << EndLn
      }
      case ast.EnumeratedType(enumerations)
      => {
        var firstIndex: Option[Long] = None
        out.ensureEmptyLines(1)
        out << "// Enumerated type" << EndLn
        out << "def encode("
        out << "value: " << safeAssignmentName
        out << "): _io_.ByteStreamer =" << EndLn
        out.indent(2) {
          out << "encode(value.asInstanceOf[_rt_.AsnEnumeration])" << EndLn
        }
        out << EndLn
        out << "def encodeData("
        out << "value: " << safeAssignmentName
        out << "): _io_.ByteStreamer =" << EndLn
        out.indent(2) {
          out << "encodeData(value.asInstanceOf[_rt_.AsnEnumeration])" << EndLn
        }
        out << EndLn
      }
      case unmatched => {
        out.ensureEmptyLines(1)
        out << "// Unmatched " << safeAssignmentName << ": " << unmatched << EndLn
      }
    }
  }
}