package org.asn1gen.gen

class AsnCodeGenerationException(
    reason: String, cause: Throwable = null)
    extends Exception(reason, cause) {
}
