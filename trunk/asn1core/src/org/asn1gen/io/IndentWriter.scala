package org.asn1gen.io

import java.io.PrintStream
import java.io.PrintWriter
import java.io.Writer

class IndentWriter(out: Writer) extends PrintWriter(out, true) {
  val defaultIndent: Int = 2
  var indent: Int = 0
  var emptyLines: Int = -1
  var line: Int = 0
  
  def atLineStart: Boolean = emptyLines != -1
  
  def this(out: PrintStream) = this(new PrintWriter(out, true))
  
  def break() = ensureEmptyLines(0)
  
  def ensureEmptyLine() = ensureEmptyLines(1)
  
  def ensureEmptyLines(lines: Int) = while (emptyLines < lines) this.println()
  
  def indent[T](offset: Int)(f: => T): T = {
    indent += offset
    try {
      return f
    } finally {
      indent -= offset
    }
  }
  
  def indent[T](f: => T): T = indent(defaultIndent)(f)
  
  override def println(): Unit = {
    super.println()
    emptyLines += 1
    line += 1
  }

  private def prePrint(): Unit = {
    if (atLineStart) {
      super.print(" " * indent)
      emptyLines = -1
    }
  }
  
  override def print(s: String): Unit = {
    if (s.length != 0) {
      prePrint()
    }
    super.print(s)
  }
  
  override def print(value: Char): Unit = {
    prePrint()
    super.print(value)
  }
  
  override def print(value: Int): Unit = {
    prePrint()
    super.print(value)
  }
  
  override def print(value: Long): Unit = {
    prePrint()
    super.print(value)
  }
  
  override def print(value: Float): Unit = {
    prePrint()
    super.print(value)
  }
  
  override def print(value: Double): Unit = {
    prePrint()
    super.print(value)
  }
}

object IndentWriter {
  def wrap[T](printWriter: PrintWriter)(f: IndentWriter => T): T = {
    val writer = new IndentWriter(printWriter)
    try {
      f(writer)
    } finally {
      writer.flush()
    }
  }
}
