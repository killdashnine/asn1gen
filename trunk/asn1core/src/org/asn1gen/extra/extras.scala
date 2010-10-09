package org.asn1gen.extra

import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.OutputStream
import java.io.PrintStream
import java.io.PrintWriter
import java.io.Writer
import org.asn1gen.io.IndentWriter

trait Extras {
  implicit def toExtra(value: Boolean) = new ExtraBoolean(value)
  
  implicit def toExtra(value: Char) = new ExtraChar(value)

  implicit def toExtra(value: String) = new ExtraString(value)

  implicit def toExtra(value: Byte) = ExtraByte(value)
  
  implicit def toExtra(value: List[Byte]) = ExtraListOfByte(value)
  
  implicit def toExtra(os: OutputStream) = ExtraOutputStream(os)
  
  implicit def toExtra(writer: Writer) = ExtraWriter(writer)
  
  implicit def toExtra(file: File) = ExtraFile(file)
  
  implicit def toExtra[T](any: T) = ExtraAnyT(any)
}

object Extras extends Extras

import org.asn1gen.extra.Extras._

case class ExtraAnyT[T](value: T) {
  def as[U](f: T => U) = f(value)
}

case class ExtraBoolean(value: Boolean) {
  def inspect: String = if (value) "true" else "false"
}

case class ExtraByte(value: Byte) {
  def definesBit(index: Int): Boolean = ((1 << index) & value) != 0
}

class ExtraChar(original: Char) {
  def isUpperHexDigit = {
    (original >= '0' && original <= '9') ||
    (original >= 'A' && original <= 'F')
  }

  def isBinDigit = {
    (original >= '0' && original <= '1')
  }
}

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
  
  def apply(path: String): File = new File(file, path)

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
  
  def child(childName: String) = new File(file, childName)
  
  def children: Array[File] = file.listFiles
  
  def children(filter: File => Boolean): Array[File] = this.children.filter(filter)
  
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

case class ExtraListOfByte(value: List[Byte]) {
  def string: String = {
    // TODO: Optimise to not create unneccessary strings.
    if (this.value.length != 0) {
      new String(this.value.toArray)
    } else {
      ""
    }
  }
}

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

class ExtraString(value: String) {
  def inspect(): String = {
    val newValue =
      ( value
      . replace("\"", "\\\"")
      . replace("\n", "\\n")
      . replace("\r", "\\r")
      . replace("\t", "\\t")
      )
    return "\"" + newValue + "\""
  }
  
  def capitalise: String = value(0).toUpper + value.substring(1)
  
  def bin: Int = {
    require(0 < value.length, "String length too small for binary")
    require(value.length <= 32, "String length too large for 32-bit binary")

    value.foldLeft(0) { case (n, c) =>
      c match {
        case '0' => n << 1
        case '1' => (n << 1) + 1
        case _ => throw new Exception("")
      }
    }
  }
}

case class ExtraWriter(writer: Writer) {
  def withIndentWriter[T](f: IndentWriter => T) = {
    val indentWriter = new IndentWriter(writer)
    try {
      f(indentWriter)
    } finally {
      indentWriter.flush()
    }
  }
}
