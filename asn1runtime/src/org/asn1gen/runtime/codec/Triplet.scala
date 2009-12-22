package org.asn1gen.runtime.codec

case class Triplet(
    tagClass: TagClass,
    constructed: Boolean,
    tagType: Int,
    length: Int) {
  def primitive = !constructed
}
