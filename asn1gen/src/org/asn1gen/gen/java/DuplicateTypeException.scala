package org.asn1gen.gen.java

class DuplicateTypeException(reason: String, cause: Throwable) extends Exception(reason, cause) {
  def this(reason: String) = this(reason, null)
}
