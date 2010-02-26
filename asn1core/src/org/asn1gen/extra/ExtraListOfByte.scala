package org.asn1gen.extra

case class ExtraListOfByte(value: List[Byte]) {
  def string: String = {
    // TODO: Optimise to not create unneccessary strings.
    if (this.value.length != 0) {
      new String(this.value.toArray)
    } else {
      ""
    }
  }
}
