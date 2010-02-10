package org.asn1gen.runtime.meta

case class AsnSequenceMember(
    name: String,
    _type: AsnType,
    optional: Boolean)
    extends AsnMember {
}
