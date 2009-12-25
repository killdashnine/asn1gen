package org.asn1gen.runtime.codec.async


import org.asn1gen.runtime.codec.DecodingInputStream

case class OnAsnBoolean(
    decoder: (OnAsnBoolean, DecodingInputStream, Int) => Unit,
    value: Boolean => Unit) extends Decodable {
  def value(transform: (Boolean => Unit) => (Boolean => Unit)): OnAsnBoolean =
    this.copy(value = transform(this.value))
  
  def decode(is: DecodingInputStream, length: Int): Unit = {
    this.decoder(this, is, length)
  }
  
  def decodeToValue(is: DecodingInputStream, length: Int): Unit = {
    require(length == 1, {"Invalid AsnBoolean encoding size"})
    val intValue = {
      val buffer = new Array[Byte](1)
      val bytesRead = is.read(buffer)
      assert(bytesRead == length)
      buffer(0) != 0
    }
    val action: Boolean => Unit = this.value
    action(intValue)
  }
}

object OnAsnBoolean extends OnAsnBoolean({_.decodeToValue(_, _)}, {_=>}){
}
