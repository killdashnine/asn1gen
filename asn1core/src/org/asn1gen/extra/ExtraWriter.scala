package org.asn1gen.extra

import java.io.Writer
import org.asn1gen.io.IndentWriter

case class WriterExtra(writer: Writer) {
  def withIndentWriter[T](f: IndentWriter => T) = {
    val indentWriter = new IndentWriter(writer)
    try {
      f(indentWriter)
    } finally {
      indentWriter.flush()
    }
  }
}
