package org.asn1gen.runtime.printing

import org.asn1gen.{runtime => _rt_}
import org.asn1gen.runtime.{meta => _meta_}

import java.io.PrintWriter
import org.asn1gen.extra.Extras
import org.asn1gen.io.IndentWriter

object SimplePrinter extends Extras {
  def print(out: IndentWriter, value: Option[_rt_.AsnType]): Unit = {
    
  }
  
  def print(out: IndentWriter, value: _rt_.AsnType): Unit = {
    value match {
      case _rt_.AsnInteger(value) => {
        out.print("AsnInteger")
        out.print("(")
        out.print(value)
        out.print(")")
      }
      case _rt_.AsnReal(value) => {
        out.print("AsnReal")
        out.print("(")
        out.print(value)
        out.print(")")
      }
      case asnChoice: _rt_.AsnChoice => {
        val line = out.line
        out.print(asnChoice._desc.name)
        out.println
        out.indent {
          out.print(".")
          out.print(asnChoice._choiceName)
          out.print(" { _ => ")
          asnChoice._element match {
            case element: _rt_.AsnType => {
              print(out, element)
            }
            case Some(element: _rt_.AsnType) => {
              out.print("Some apply ")
              print(out, element)
            }
          }
          if (line != out.line) {
            out.break()
          }
          out.println("}")
        }
      }
      case asnCharacterString: _rt_.AsnCharacterString => {
        out.print(asnCharacterString._desc.name)
        if (asnCharacterString.length != 0) {
          out.print("(" + asnCharacterString.value.inspect() + ")")
        }
      }
      case _rt_.AsnOctetString(bytes) => {
        out.print("AsnOctetString")
        if (bytes.length != 0) {
          out.print("(" + bytes.string.inspect() + ")")
        }
      }
      case asnSequence: _rt_.AsnSequence => {
        val desc = asnSequence._desc
        out.print(desc.name)
        out.indent {
          desc.children.foreach { case (name, _: _meta_.AsnSequenceMember) =>
            out.break()
            val child = asnSequence._child(name)
            child match {
              case None =>
              case Some(subValue: _rt_.AsnSequence) => {
                out.print(".")
                out.print(name)
                out.print(" { _ => Some apply ")
                val line = out.line
                this.print(out, subValue)
                if (line != out.line) {
                  out.break()
                  out.print("}")
                } else {
                  out.print(" }")
                }
              }
              case Some(subValue: _rt_.AsnType) => {
                out.print(".")
                out.print(name)
                out.print(" { _ => Some apply ")
                val line = out.line
                this.print(out, subValue)
                out.print("}")
              }
              case subValue: _rt_.AsnType => {
                out.print(".")
                out.print(name)
                out.print(" { _ => ")
                val line = out.line
                this.print(out, subValue)
                if (line != out.line) {
                  out.break()
                  out.print("}")
                } else {
                  out.print(" }")
                }
              }
            }
          }
        }
      }
      case _rt_.AsnBoolean(value) => {
        out.print("AsnBoolean")
        out.print("(")
        out.print(value)
        out.print(")")
      }
      case enumeration: _rt_.AsnEnumeration => {
        out.print(enumeration._desc.name)
        enumeration._shortName match {
          case Some(shortName) => {
            out.print(".")
            out.print(shortName)
          }
          case _ => {
            out.print("(")
            out.print(enumeration._value)
            out.print(")")
          }
        }
      }
      case _ => {
        out.print("/**")
        out.print(value)
        out.print("**/")
      }
    }
  }
}
