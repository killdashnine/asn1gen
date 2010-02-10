package org.asn1gen.runtime

trait AsnType {
  def _desc: meta.AsnType = meta.AsnType

  def _child(name: String): Any = {
    throw new Exception("No such field '" + name + "'")
  }
}

object AsnType {
}
