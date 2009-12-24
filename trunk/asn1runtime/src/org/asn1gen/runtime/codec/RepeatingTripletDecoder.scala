package org.asn1gen.runtime.codec

class RepeatingTripletDecoder(is: DecodingInputStream, endIndex: Int) extends BerDecoderBase {
  var triplet: Option[Triplet] = None
  
  def decode(f: PartialFunction[Option[Triplet], Unit]) = {
    assert(is.index <= endIndex)
    if (triplet == None && is.index < endIndex) {
      triplet = Some(decodeTriplet(is))
    }
    val result = f.lift(triplet)
    if (result != None) {
      triplet = None
    }
  }
}

