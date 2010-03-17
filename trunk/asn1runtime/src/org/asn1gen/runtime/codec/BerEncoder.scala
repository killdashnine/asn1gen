package org.asn1gen.runtime.codec

import java.io._
import org.asn1gen.io._
import org.asn1gen.runtime._

trait BerEncoder {
  def encodeTagType(tagType: Int): ByteStreamer = {
    if (tagType < 0x80) {
      ByteStreamer.byte(tagType.toByte)
    } else {
      encodeTagType(tagType >> 7) ::: ByteStreamer.byte((tagType & 0x7f).toByte) 
    }
  }
  
  def encodeTagHeader(tagHeader: TagHeader): ByteStreamer = {
    val tagClassPart = tagHeader.tagClass.value & 0x3 << 5
    val tagConstructedPart = if (tagHeader.constructed) 0x10 else 0
    val tagTypePart = tagHeader.tagType
    if (tagHeader.tagType < 30) {
      val headerByte = (tagClassPart | tagConstructedPart | tagTypePart).toByte
      ByteStreamer.byte(headerByte)
    } else {
      val headerLeaderByte = (tagClassPart | tagConstructedPart | 31).toByte
      ByteStreamer.byte(headerLeaderByte)
    }
  }
  
  def encode(value: AsnNull): ByteStreamer = ByteStreamer.nil
  
  def encode(value: Boolean): ByteStreamer = {
    ByteStreamer.byte(if (value) (-1).toByte else 0.toByte)
  }
  
  def encodeFixedMore(value: Long): ByteStreamer = {
    if (value == 0) {
      ByteStreamer.byte(0)
    } else if (value == -1) {
      ByteStreamer.byte(-1)
    } else {
      val b: Byte = value.toByte
      if ((value & 0xffffffffffffff80L) == 0) {
        ByteStreamer.byte(b)
      } else if ((value & 0xffffffffffffff80L) == 0xffffffffffffff80L) {
        ByteStreamer.byte(b)
      } else {
        encodeFixedMore(value >> 8) ::: ByteStreamer.byte(b)
      }
    }
  }
  
  def encodeFixed(value: Long): ByteStreamer = {
    if (value == -1L) {
      ByteStreamer.byte(-1)
    } else {
      encodeFixedMore(value)
    }
  }
  
  def encodeRaw(value: String): ByteStreamer = {
    ByteStreamer.bytes(value.getBytes)
  }
  
  def encodeLengthMore(value: Int): ByteStreamer = {
    if (value == 0) {
      ByteStreamer.nil
    } else if (value >= 256) {
      encodeLengthMore(value >> 8) ::: ByteStreamer.byte((value & 0xff).toByte)
    } else {
      ByteStreamer.byte(value.toByte)
    }
  }
  
  def encodeLength(value: Int): ByteStreamer = {
    if (value < 0) {
      throw new EncodingException("length may not be negative")
    } else if (value < 128) {
      ByteStreamer.byte(value.toByte)
    } else {
      val lengthPart = encodeLengthMore(value)
      ByteStreamer.byte((0x80 | lengthPart.length).toByte) ::: lengthPart
    }
  }
}

object BerEncoder extends BerEncoder
