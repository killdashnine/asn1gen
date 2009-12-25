package org.asn1gen.runtime.codec.async

import org.asn1gen.runtime.codec.DecodingInputStream

trait Decodable {
  def decode(is: DecodingInputStream, length: Int): Unit
}
