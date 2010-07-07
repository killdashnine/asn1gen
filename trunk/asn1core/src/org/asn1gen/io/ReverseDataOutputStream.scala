package org.asn1gen.io

import java.io._
import scala.util.control.Breaks

class ReverseDataOutputStream(out: OutputStream)
    extends FilterOutputStream(out) with DataOutput {
  protected var written: Int = 0

  private var bytearr: Array[Byte] = null 

  protected def incCount(value: Int): Unit = {
    val temp = written + value
    if (temp < 0) {
      written = Int.MaxValue
    } else {
      written = temp
    }
  }

  override def write(byte: Int): Unit = {
    this.synchronized {
      out.write(byte)
      incCount(1)
    }
  }

  override def write(buffer: Array[Byte], offset: Int, length: Int): Unit = {
    this.synchronized {
      out.write(buffer, offset, length)
      incCount(length)
    }
  }

  override def flush: Unit = {
    out.flush()
  }

  final def writeBoolean(value: Boolean): Unit = {
    out.write(if (value) 1 else 0)
    incCount(1)
  }

  final def writeByte(value: Int): Unit = {
    out.write(value)
    incCount(1)
  }

  final def writeShort(value: Int): Unit = {
    out.write((value >>> 8) & 0xFF)
    out.write((value >>> 0) & 0xFF)
    incCount(2)
  }

  final def writeChar(value: Int): Unit = {
    out.write((value >>> 8) & 0xFF)
    out.write((value >>> 0) & 0xFF)
    incCount(2)
  }

  final def writeInt(value: Int): Unit = {
    out.write((value >>> 24) & 0xFF)
    out.write((value >>> 16) & 0xFF)
    out.write((value >>>  8) & 0xFF)
    out.write((value >>>  0) & 0xFF)
    incCount(4)
  }

  final def writeLong(value: Long): Unit = {
    val writeBuffer = new Array[Byte](8)
    writeBuffer(0) = (value >>> 56).toByte
    writeBuffer(1) = (value >>> 48).toByte
    writeBuffer(2) = (value >>> 40).toByte
    writeBuffer(3) = (value >>> 32).toByte
    writeBuffer(4) = (value >>> 24).toByte
    writeBuffer(5) = (value >>> 16).toByte
    writeBuffer(6) = (value >>>  8).toByte
    writeBuffer(7) = (value >>>  0).toByte
    out.write(writeBuffer, 0, 8)
    incCount(8);
  }

  final def writeFloat(value: Float): Unit = {
    writeInt(java.lang.Float.floatToIntBits(value))
  }

  final def writeDouble(value: Double): Unit = {
    writeLong(java.lang.Double.doubleToLongBits(value))
  }

  final def writeBytes(value: String): Unit = {
    val length = value.length
    for (i <- 0 to (length - 1)) {
      out.write(value.charAt(i))
    }
    incCount(length);
  }

  final def writeChars(value: String): Unit = {
    val length = value.length
    for (i <- 0 to (length - 1)) {
      val char = value.charAt(i)
      out.write((char >>> 8) & 0xFF) 
      out.write((char >>> 0) & 0xFF) 
    }
    incCount(length * 2)
  }

  final def writeUTF(value: String): Unit = {
    writeUTF(value, this)
  }

  final def writeUTF(value: String, out: DataOutput): Int = {
    val strlen = value.length
    var utflen = 0
    var count = 0

    // Use charAt instead of copying String to char array
    for (i <- 0 to (strlen - 1)) {
      val c = value.charAt(i)
      if ((c >= 0x0001) && (c <= 0x007F)) {
        utflen += 1
      } else if (c > 0x07FF) {
        utflen += 3;
      } else {
        utflen += 2;
      }
    }

    if (utflen > 65535) {
      throw new UTFDataFormatException(
          "encoded string too long: " + utflen + " bytes");
    }
    val bytearr: Array[Byte] =
      if (out.isInstanceOf[DataOutputStream]) {
        val dos = out.asInstanceOf[ReverseDataOutputStream]
        if (dos.bytearr == null || (dos.bytearr.length < (utflen + 2))) {
            dos.bytearr = new Array[Byte]((utflen * 2) + 2)
        }
        dos.bytearr;
      } else {
        new Array[Byte](utflen + 2)
      }
   
      bytearr(count) = ((utflen >>> 8) & 0xFF).toByte
      count += 1
      bytearr(count) = ((utflen >>> 0) & 0xFF).toByte
      count += 1
      
      var i: Int = 0
      val mybreaks = new Breaks
      import mybreaks.{break, breakable}
      breakable {
        for (i <- 0 to (strlen - 1)) {
           val c = value.charAt(i);
           if (!((c >= 0x0001) && (c <= 0x007F))) {
             break
           }
           bytearr(count) = c.toByte
           count += 1
        }
      }

      for (i <- i to (strlen - 1)){
        val c = value.charAt(i)
        if ((c >= 0x0001) && (c <= 0x007F)) {
          bytearr(count) = c.toByte
          count += 1
        } else if (c > 0x07FF) {
          bytearr(count) = (0xE0 | ((c >> 12) & 0x0F)).toByte
          count += 1
          bytearr(count) = (0x80 | ((c >>  6) & 0x3F)).toByte
          count += 1
          bytearr(count) = (0x80 | ((c >>  0) & 0x3F)).toByte
          count += 1
        } else {
          bytearr(count) = (0xC0 | ((c >>  6) & 0x1F)).toByte
          count += 1
          bytearr(count) = (0x80 | ((c >>  0) & 0x3F)).toByte
          count += 1
        }
    }
    out.write(bytearr, 0, utflen + 2)
    return utflen + 2
  }

  final def size(): Int = {
    return written
  }
}
