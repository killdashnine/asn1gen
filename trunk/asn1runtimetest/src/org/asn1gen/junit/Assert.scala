package org.asn1gen.junit

object Assert {
  def assertThrows[E](f: => Unit): Unit = {
    try {
      f
    } catch {
      case e: E => return
      case e => throw e
    }
    throw new Exception("Exception expected")
  }
}
