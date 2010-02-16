package org.asn1gen.io

import java.io.PrintStream
import java.io.PrintWriter
import java.io.Writer

class IndentWriter(out: Writer) extends PrintWriter(out, true) {
  val defaultIndent: Int = 2
  var indent: Int = 0
  var emptyLines: Int = 0
  
  def atLineStart: Boolean = emptyLines != 0
  
  def this(out: PrintStream) = this(new PrintWriter(out, true))
  
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
  }
  
  override def print(s: String): Unit = {
    if (s.length != 0) {
      if (atLineStart) {
        super.print(" " * indent)
        emptyLines = 0
      }
    }
    super.print(s)
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
