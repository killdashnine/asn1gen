package org.asn1gen.gen.scala

class DuplicateValueException(reason: String, cause: Throwable) extends Exception(reason, cause) {
  def this(reason: String) = this(reason, null)
}
