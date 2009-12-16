package org.asn1gen.runtime.codec

case class OctetWindow(buffer: Array[Byte], start: Int, length: Int) {
  assert(start >= 0 && length >= 0 && start + length <= buffer.length)
  
  def apply(index: Int): Byte = {
    assert(index >= 0 && index < length)
    buffer(start + index)
  }
  
  def zoom(start: Int, length: Int): OctetWindow = {
    OctetWindow(buffer, start + this.start, length)
  }
}
