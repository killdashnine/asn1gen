package org.asn1gen.extra

import java.io.OutputStream
import java.io.PrintWriter
import org.asn1gen.io.IndentWriter
import org.asn1gen.extra.Extras._

case class ExtraOutputStream(os: OutputStream) {
  def withPrintWriter[T](f: PrintWriter => T) = {
    val printWriter = new PrintWriter(os)
    try {
      f(printWriter)
    } finally {
      printWriter.flush()
    }
  }

  def withIndentWriter[T](f: IndentWriter => T) = {
    this.withPrintWriter { printWriter =>
      printWriter.withIndentWriter(f)
    }
  }
}
