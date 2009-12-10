package org.asn1gen.runtime

case class AsnChoice(choice: AsnType) extends AsnType {
  def choice_ = choice
}
