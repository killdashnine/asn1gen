package org.asn1gen.runtime.codec

sealed abstract class TagClass(val value: Int) {
}

object TagClass {
  object Universal extends TagClass(0)
  object Application extends TagClass(1)
  object ContextSpecific extends TagClass(2)
  object Private extends TagClass(3)
}
