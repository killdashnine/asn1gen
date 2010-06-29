package org.asn1gen.io

abstract class ByteStreamer(val length: Int) extends (List[Byte] => List[Byte]) {
  def ::(byte: Byte) = new ByteStreamer(this.length + 1) {
    def apply(tail: List[Byte]): List[Byte] = byte :: tail
  }
  
  def :::(head: ByteStreamer): ByteStreamer = {
    val tail = this
    new ByteStreamer(tail.length + head.length) {
      def apply(rest: List[Byte]): List[Byte] = head(tail(rest))
    }
  }
}

object ByteStreamer {
  object nil extends ByteStreamer(0) {
    def apply(tail: List[Byte]): List[Byte] = tail
  }

  val byte = {
    0.to(0xff).map { value =>
      new ByteStreamer(1) {
        def apply(tail: List[Byte]): List[Byte] = value.toByte :: tail
      }
    }
  }

  def bytes(values: List[Byte]) = new ByteStreamer(values.length) {
    def apply(tail: List[Byte]): List[Byte] = values ::: tail
  }
  
  def bytes(values: Byte*): ByteStreamer = bytes(values.toList)
  
  def bytes(values: Array[Byte]): ByteStreamer = bytes(values.toList)
}
