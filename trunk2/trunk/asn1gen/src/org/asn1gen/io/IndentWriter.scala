package org.asn1gen.io

import java.io.PrintStream
import java.io.PrintWriter
import java.io.Writer

class IndentWriter(out: Writer) extends PrintWriter(out, true) {
  private var indent_ : Int = 0
  private var lineStart_ : Boolean = true
  
  def this(out: PrintStream) = this(new PrintWriter(out, true))
  
  def indent[T](offset: Int)(f: => T): T = {
    indent_ += offset
    try {
      return f
    } finally {
      indent_ -= offset
    }
  }
  
  override def println(): Unit = {
    super.println()
    lineStart_ = true
  }
  
  override def print(s: String): Unit = {
    if (lineStart_) {
      super.print(" " * indent_)
      lineStart_ = false
    }
    super.print(s)
  }
  
  def indent: Int = indent_
}
