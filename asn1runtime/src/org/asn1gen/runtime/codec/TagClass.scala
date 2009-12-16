package org.asn1gen.runtime.codec

sealed abstract class TagClass {
}

object TagClass {
  object Universal extends TagClass
  object Application extends TagClass
  object ContextSpecific extends TagClass
  object Private extends TagClass
}
