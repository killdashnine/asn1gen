package org.asn1gen.runtime.codec

sealed abstract class TagClass(val value: Int) {
  val bits: Int = (value & 0x3) << 6 
}

object TagClass {
  object Universal extends TagClass(0)
  object Application extends TagClass(1)
  object ContextSpecific extends TagClass(2)
  object Private extends TagClass(3)
}
