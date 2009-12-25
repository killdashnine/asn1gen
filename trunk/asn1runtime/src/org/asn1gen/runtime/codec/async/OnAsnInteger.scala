package org.asn1gen.runtime.codec.async

import org.asn1gen.runtime.codec.DecodingInputStream

case class OnAsnInteger(value: Long => Unit) extends Decodable {
  def value(transform: (Long => Unit) => (Long => Unit)): OnAsnInteger =
    OnAsnInteger(transform(this.value))
  
  def decode(is: DecodingInputStream, length: Int): Unit = {
    val intValue =
      if (length == 0) {
        0
      } else {
        val buffer = new Array[Byte](length)
        val bytesRead = is.read(buffer)
        assert(bytesRead == length)
        var acc = if (buffer(0) >= 0) 0L else -1L
        buffer foreach { byte =>
          acc = (acc << 8) | byte
        }
        acc
      }
    val action: Long => Unit = this.value
    action(intValue)
  }
}

object OnAsnInteger extends OnAsnInteger({_=>}){
}
