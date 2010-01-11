package org.asn1gen.io

import java.io.FileOutputStream
import java.io.OutputStream
import java.io.PrintStream
import org.asn1gen.io.JavaTypes._

case class RichFile(file: JavaFile) {
  def openOutputStream[T](f: OutputStream => T): T = {
    val os = new FileOutputStream(file)
    try f(os) finally os.close
  }
  
  def openPrintStream[T](f: PrintStream => T): T = {
    openOutputStream { os =>
      val ps = new PrintStream(os)
      try f(ps) finally ps.flush
    }
  }
}

object RichFile {
  implicit def fromJava(file: java.io.File): RichFile = RichFile(file)
  implicit def toJava(file: RichFile): java.io.File = file.file
}
