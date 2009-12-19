package org.asn1gen.runtime.codec

case class Triplet(window: OctetWindow) {
  import org.asn1gen.extra.Extras._
  
  lazy val tagClass: TagClass = {
    (window(0) >> 6) match {
      case 0 => TagClass.Universal
      case 1 => TagClass.Application
      case 2 => TagClass.ContextSpecific
      case 3 => TagClass.Private
    }
  }
  
  lazy val tagPrimitive: Boolean = {
    ((window(0) >> 5) & 1) == 0
  }
  
  lazy val tagConstructed = !tagPrimitive
  
  lazy val tagValueAndLength: (Int, Int) = {
    var index = 0
    
    // Read tag bytes
    var tagValue = window(index) & 0x1f
    index += 1
    if (tagValue > 30) {
      while (window(index) definesBit 7) {
        tagValue = (tagValue << 7) | (window(index) & 0x7f)
        index += 1
      }
      
      tagValue = (tagValue << 7) | (window(index) & 0x7f)
      index += 1
    }
    
    // Read length bytes
    val lengthByte = window(index)
    index += 1
    val length = (
      if (lengthByte definesBit 7) {
        val lengthSize = lengthByte & 0x7f
        if (lengthSize == 0) {
          throw new Exception("Indefinite length currently not supported")
        }
        
        var partialLength = 0
        (0 until lengthSize) foreach { i =>
          partialLength = partialLength << 8
          partialLength += window(index)
          index += 1
        }
        
        partialLength
      } else {
        lengthByte
      }
    )
    
    (tagValue, length)
  }
  
  lazy val tagValue = tagValueAndLength._1
  
  lazy val length = tagValueAndLength._2
}
