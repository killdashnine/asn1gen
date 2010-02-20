package org.asn1gen.extra

import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream
import java.io.PrintWriter
import org.asn1gen.io.IndentWriter
import org.asn1gen.extra.Extras._

case class ExtraFile(file: File) {
  def withOutputStream[T](f: OutputStream => T) = {
    val out = new FileOutputStream(file)
    try {
      f(out)
    } finally {
      out.close()
    }
  }
  
  def withPrintWriter[T](f: PrintWriter => T) = {
    this.withOutputStream { _.withPrintWriter(f) }
  }

  def withIndentWriter[T](f: IndentWriter => T) = {
    this.withPrintWriter { _.withIndentWriter(f) }
  }
  
  def /(path: String): File = new File(file, path)
}
