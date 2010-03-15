package org.asn1gen.io

abstract class ByteStreamer(val length: Int) extends (List[Byte] => List[Byte]) {
  def ::(byte: Byte) = new ByteStreamer(this.length + 1) {
    def apply(tail: List[Byte]): List[Byte] = byte :: tail
  }
  
  def :::(tailStreamer: ByteStreamer): ByteStreamer = {
    new ByteStreamer(this.length + tailStreamer.length) {
      def apply(tail: List[Byte]): List[Byte] = this(tailStreamer(tail))
    }
  }
}

object ByteStreamer {
  object nil extends ByteStreamer(0) {
    def apply(tail: List[Byte]): List[Byte] = tail
  }
  
  def byte(value: Byte) = new ByteStreamer(1) {
    def apply(tail: List[Byte]): List[Byte] = value :: tail
  }
  
  def bytes(values: List[Byte]) = new ByteStreamer(values.length) {
    def apply(tail: List[Byte]): List[Byte] = values ::: tail
  }
  
  def bytes(values: Byte*): ByteStreamer = bytes(values.toList)
}
