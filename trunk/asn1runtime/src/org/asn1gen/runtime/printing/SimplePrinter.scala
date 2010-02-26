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
      case asnCharacterString: _rt_.AsnCharacterString => {
        "".inspect()
        out.print(asnCharacterString._desc.name + "(" + asnCharacterString.value.inspect() + ")")
      }
      case asnOctetString: _rt_.AsnOctetString => {
        out.print("AsnOctetString(" + asnOctetString.string.inspect() + ")")
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
                out.print(" { _ => Some(")
                val line = out.line
                this.print(out, subValue)
                out.print(") }")
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
      case _rt_.AsnBoolean => out.print("AsnBoolean")
      case _ => {
        out.print("/**")
        out.print(value)
        out.print("**/")
      }
    }
  }
}