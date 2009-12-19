package org.asn1gen.runtime.codec

import org.asn1gen.runtime._
import org.asn1gen.extra._
import java.io._

class BerDecoder {
  def tripletFor(window: OctetWindow): Triplet = {
    Triplet(window)
  }
  
  def decodeAs(triplet: Triplet, value: AsnInteger): AsnInteger = {
    assert(triplet.tagValue == 2)
    AsnInteger(1)
  }
  
  def decode(window: OctetWindow): AsnType = {
    null
  }
  
  def decode(is: InputStream): AsnType = {
    decode(BerDecoderReader(is).readTripletWindow())
  }
}
