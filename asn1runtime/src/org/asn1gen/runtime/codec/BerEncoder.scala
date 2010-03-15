package org.asn1gen.runtime.codec

import java.io._
import org.asn1gen.io._

trait BerEncoder {
  def encodeFixedMore(value: Long): ByteStreamer = {
    if (value == 0) {
      ByteStreamer.byte(0)
    } else if (value == -1) {
      ByteStreamer.nil
    } else {
      val b: Byte = value.toByte
      if (value < 128) {
        ByteStreamer.byte(b)
      } else {
        encodeFixedMore(value >> 8) ::: ByteStreamer.byte(b)
      }
    }
  }
  
  def encodeFixed(value: Long): ByteStreamer = {
    if (value == -1L) {
      ByteStreamer.byte(-1)
    } else if (value == 0L) {
      ByteStreamer.nil
    } else {
      encodeFixedMore(value)
    }
  }
}

object BerEncoder extends BerEncoder
