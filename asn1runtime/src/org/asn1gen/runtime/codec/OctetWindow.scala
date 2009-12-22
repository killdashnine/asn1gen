package org.asn1gen.runtime.codec

case class OctetWindow(buffer: Array[Byte], start: Int, length: Int) {
  assert(start >= 0 && length >= 0 && start + length <= buffer.length)
  
  def apply(index: Int): Byte = {
    assert(index >= 0 && index < length)
    buffer(start + index)
  }
  
  def end: Int = start + length
  
  def contains(window: OctetWindow): Boolean = {
    assert(this.buffer == window.buffer)
    return this.start <= window.start && this.end >= window.end
  }
  
  def zoomAbsolute(start: Int, length: Int): OctetWindow = {
    val window = OctetWindow(buffer, start, length)
    assert(contains(window))
    window
  }
  
  def zoom(start: Int, length: Int): OctetWindow = {
    zoomAbsolute(this.start + start, length)
  }
  
  def from(start: Int): OctetWindow = {
    zoomAbsolute(this.start + start, this.length - start)
  }
  
  def until(length: Int): OctetWindow = {
    zoomAbsolute(this.start, length)
  }
}
