package org.asn1gen.runtime.codec

import java.io._
import org.asn1gen.io._

trait BerEncoder {
  def encodeFixedInteger(value: Long): ByteStreamer = {
    val b: Byte = value.toByte
    if (value == 0) {
      ByteStreamer.nil
    } else if (value < 256) {
      ByteStreamer.byte(b)
    } else {
      encodeFixedInteger(value >> 8) ::: ByteStreamer.byte(b)
    }
  }
}
