package org.asn1gen.runtime.codec.async

import org.asn1gen.runtime.codec.DecodingException
import org.asn1gen.runtime.codec.DecodingInputStream

trait Decodable {
  def decode(is: DecodingInputStream, length: Int): Unit
  
  @throws(classOf[DecodingException])
  def require(condition: Boolean, reason: => String): Boolean = {
    if (!condition) {
      throw new DecodingException(reason)
    } else {
      condition
    }
  }
}
