package org.asn1gen.runtime.codec.async

import org.asn1gen.runtime.codec.DecodingInputStream

case class OnAsnInteger(
    decoder: (OnAsnInteger, DecodingInputStream, Int) => Unit,
    value: Long => Unit) extends Decodable {
  type Decoder = (OnAsnInteger, DecodingInputStream, Int) => Unit
  
  def decoder(transform: Decoder => Decoder): OnAsnInteger =
    this.copy(decoder = transform(this.decoder))
  
  def value(transform: (Long => Unit) => (Long => Unit)): OnAsnInteger =
    this.copy(value = transform(this.value))
  
  def decode(is: DecodingInputStream, length: Int): Unit = {
    this.decoder(this, is, length)
  }
  
  def decodeToValue(is: DecodingInputStream, length: Int): Unit = {
    require(length != 0, {"Zero length integer found."})
    val intValue = {
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

object OnAsnInteger extends OnAsnInteger({_.decodeToValue(_, _)}, {_=>}){
}
