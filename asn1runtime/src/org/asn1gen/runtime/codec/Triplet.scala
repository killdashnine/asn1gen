package org.asn1gen.runtime.codec

import org.asn1gen.runtime._

case class Triplet(
    tagClass: TagClass,
    constructed: Boolean,
    tagType: Int,
    length: Int) {
  def primitive = !constructed
  
  def describes(template: AsnNull) =
    primitive && tagType == 5 && length == 0
  
  def describes(template: AsnBoolean) =
    primitive && tagType == 1 && length == 1
  
  def describes(template: AsnInteger) =
    primitive && tagType == 2 && length > 0
  
  def describes(template: AsnSequence) =
    constructed && tagClass == TagClass.Universal && tagType == 16
}
