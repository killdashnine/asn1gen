package org.asn1gen.runtime

class BadEnumerationException(reason: String, cause: Throwable) extends AsnException(reason, cause) {
}

object BadEnumerationException {
  def apply(reason: String, cause: Throwable) = new BadEnumerationException(reason, cause)
  
  def apply(reason: String) = new BadEnumerationException(reason, null)
}
