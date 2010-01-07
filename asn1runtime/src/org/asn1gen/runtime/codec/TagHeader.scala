package org.asn1gen.runtime.codec

import org.asn1gen.runtime._

case class TagHeader(
    tagClass: TagClass,
    constructed: Boolean,
    tagType: Int) {
  def primitive = !constructed
  
  def contextSpecific = tagClass == TagClass.ContextSpecific
  
  def describes(template: AsnNull): Boolean =
    primitive && tagType == 5
  
  def describes(template: AsnBoolean): Boolean =
    primitive && tagType == 1
  
  def describes(template: AsnInteger): Boolean =
    primitive && tagType == 2
  
  def describes(template: AsnSequence): Boolean =
    constructed && tagClass == TagClass.Universal && tagType == 16
}
