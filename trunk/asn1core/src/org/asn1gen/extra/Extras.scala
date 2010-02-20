package org.asn1gen.extra

import java.io.File
import java.io.OutputStream
import java.io.Writer

trait Extras {
  implicit def toExtra(value: Char) = new ExtraChar(value)

  implicit def toExtra(value: String) = new ExtraString(value)

  implicit def toExtra(value: Byte) = ByteExtra(value)
  
  implicit def toExtra(os: OutputStream) = OutputStreamExtra(os)
  
  implicit def toExtra(writer: Writer) = WriterExtra(writer)
  
  implicit def toExtra(file: File) = ExtraFile(file)
}

object Extras extends Extras
