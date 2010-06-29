package org.asn1gen.runtime.codec

import java.io._
import org.asn1gen.io._
import org.asn1gen.runtime._

trait BerEncoder {
  def encodeTagType(tagType: Int): ByteStreamer = {
    if (tagType < 0x80) {
      ByteStreamer.byte(tagType)
    } else {
      encodeTagType(tagType >> 7) ::: ByteStreamer.byte(tagType & 0x7f) 
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
  
  def encodeFixedMore(value: Long): ByteStreamer = {
    if (value == 0) {
      ByteStreamer.byte(0x00)
    } else if (value == -1) {
      ByteStreamer.byte(0xff)
    } else {
      if ((value & 0xffffffffffffff80L) == 0) {
        ByteStreamer.byte((value & 0xff).toInt)
      } else if ((value & 0xffffffffffffff80L) == 0xffffffffffffff80L) {
        ByteStreamer.byte((value & 0xff).toInt)
      } else {
        encodeFixedMore(value >> 8) ::: ByteStreamer.byte((value & 0xff).toInt)
      }
    }
  }
  
  def encodeFixed(value: Long): ByteStreamer = {
    if (value == -1L) {
      ByteStreamer.byte(0xff)
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
      encodeLengthMore(value >> 8) ::: ByteStreamer.byte(value & 0xff)
    } else {
      ByteStreamer.byte(value & 0xff)
    }
  }
  
  def encodeLength(value: Int): ByteStreamer = {
    if (value < 0) {
      throw new EncodingException("length may not be negative")
    } else if (value < 128) {
      ByteStreamer.byte(value & 0xff)
    } else {
      val lengthPart = encodeLengthMore(value)
      ByteStreamer.byte(0x80 | lengthPart.length) ::: lengthPart
    }
  }
  
  def encode(value: String): ByteStreamer = {
    // TODO: Implement me
    ByteStreamer.nil
  }
  
  ///////
  ///////

  /**
   * Encode the data part of a boolean value.
   * The value false is represented by 0x00.  The value true may be
   * represented by any other value, but this implementation will encode it
   * as 0xff.
   * @param value
   *  The value to encode
   * @return
   *  The encoded data.
   */
  def encodeData(value: Boolean): ByteStreamer = {
    ByteStreamer.byte(if (value) 0xff else 0)
  }
  
  /**
   * Encode the header and data part of a boolean value.
   * @param value
   *  The value to encode.
   * @return
   *  The encoded header and data.
   */
  def encode(value: Boolean): ByteStreamer = {
    val tag = ByteStreamer.byte(1)
    val length = ByteStreamer.byte(1)
    val data = encodeData(value)
    tag ::: length ::: data
  }

  /**
   * Encode the data part of an null value.
   * The value is presented by a zero-length value.
   * @param value
   *  The value to encode.
   * @return
   *  The encoded data.
   */
  def encodeData(value: AsnNull): ByteStreamer = {
    ByteStreamer.nil
  }

  /**
   * Encode the header and data part of a null value.
   * @param value
   *  The value to encode.
   * @return
   *  The encoded header and data.
   */
  def encode(value: AsnNull): ByteStreamer = {
    val tag = ByteStreamer.byte(5)
    val length = ByteStreamer.byte(0)
    tag ::: length
  }
  
  /**
   * Encode the data part of an integer value.
   * This implementation uses the shortest representation allowed.
   * @param value
   *  The value to encode.
   * @return
   *  The encoded data.
   */
  def encodeData(value: Long): ByteStreamer = {
    val leader = value >> 7
    if (leader == -1 || leader == 0) {
      ByteStreamer.byte(value.toInt)
    } else {
      // The value is greater than a byte so must encode the last byte and
      // the remainder separately.
      val lastByte = ByteStreamer.byte((value & 0xff).toInt)
      val remainder = encodeData(value >> 8)
      remainder ::: lastByte
    }
  }

  /**
   * Encode the header and data part of a null value.
   * @param value
   *  The value to encode.
   * @return
   *  The encoded header and data.
   */
  def encode(value: Long): ByteStreamer = {
    val tag = ByteStreamer.byte(2)
    val data = encodeData(value)
    val length = ByteStreamer.byte(data.length)
    tag ::: length ::: data
  }
}

object BerEncoder extends BerEncoder
