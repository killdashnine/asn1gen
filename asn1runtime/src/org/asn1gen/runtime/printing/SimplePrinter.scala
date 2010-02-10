package org.asn1gen.runtime.printing

import org.asn1gen.{runtime => _rt_}
import org.asn1gen.runtime.{meta => _meta_}

import java.io.PrintWriter

object SimplePrinter {
  def print(out: PrintWriter, value: Any): Unit = {
    value match {
      case asnSequence: _rt_.AsnSequence => {
        val desc = asnSequence._desc
        out.print(desc.name)
        desc.children.foreach { case (name, member: _meta_.AsnSequenceMember) =>
          out.print(".")
          out.print(name)
          out.print("{ _ => ")
          this.print(out, asnSequence._child(name))
          out.print("}")
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
