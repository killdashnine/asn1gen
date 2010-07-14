package org.asn1gen.runtime.codec

sealed class TagConstruction(value: Boolean) {
  val bits: Int = if (value) 1 << 5 else 0
}

object TagPrimitive extends TagConstruction(false)
object TagConstructed extends TagConstruction(true)
