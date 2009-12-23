package org.asn1gen.junit

trait Assert {
  def assertThrows[E](eClass: Class[E])(f: => Unit): Unit = {
    try {
      f
    } catch {
      case e if eClass.isAssignableFrom(e.getClass) => return
      case e => throw e
    }
    
    throw new Exception("Exception expected")
  }
}

object Assert extends Assert
