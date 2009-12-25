package org.asn1gen.runtime.codec

class DecodingException(reason: String, cause: Throwable) extends Exception(reason, cause) {
  def this(reason: String) = this(reason, null)
}
