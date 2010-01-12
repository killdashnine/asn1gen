package org.asn1gen.io

import java.io.FileOutputStream
import java.io.IOException
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
  
  def child(childName: String) = new JavaFile(file, childName)
  
  def children: Array[JavaFile] = file.listFiles
  
  def children(filter: JavaFile => Boolean): Array[JavaFile] = this.children.filter(filter)
  
  def requireExists = {
    if (!file.exists) {
      throw new IOException("Directory does not exist")
    }
  }
  
  def requireDirectory = {
    if (!file.isDirectory) {
      throw new IOException("'" + file.getName + "' is not a directory")
    }
  }
  
  def name = file.getName
}

object RichFile {
  implicit def fromJava(file: java.io.File): RichFile = RichFile(file)
  implicit def toJava(file: RichFile): java.io.File = file.file
}
