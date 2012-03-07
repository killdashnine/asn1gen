package test.org.asn1gen.runtime.codec

import org.asn1gen.runtime._
import org.asn1gen.runtime.codec._
import org.asn1gen.runtime.codec.async._
import java.io._
import org.junit._
import org.junit.Assert._
import org.asn1gen.junit.Assert._
import test.asn1.genruntime.BerDecoder
import _root_.org.asn1gen.io._
import _root_.org.asn1gen.runtime.codec.BerEncoder._
import org.junit.runner.RunWith

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class  TestBerEncoder extends org.specs2.mutable.SpecificationWithJUnit {
  "Encoder for 0" >> {
    val encoder = encodeFixed(0)
    
    "should encode to List[Byte](0)" >> {
      encoder(Nil) must_== List[Byte](0)
    }
  }
  
  "Encoder for -1" >> {
    val encoder = encodeFixed(-1)

    "should encode to List[Byte](-1)" >> {
      encoder(Nil) must_== List[Byte](-1)
    }
  }
  
  "Encoder for 42" >> {
    val encoder = encodeFixed(42)

    "should encode to List[Byte](42)" >> {
      encoder(Nil) must_== List[Byte](42)
    }
  }

  "Encoder for -42" >> {
    val encoder = encodeFixed(-42)

    "should encode to List[Byte](-42)" >> {
      encoder(Nil) must_== List[Byte](-42)
    }
  }

  "Encoder for 255" >> {
    val encoder = encodeFixed(255)

    "should encode to List[Byte](0, -1)" >> {
      encoder(Nil) must_== List[Byte](0, -1)
    }
  }

  "Encoder for 128" >> {
    val encoder = encodeFixed(128)

    "should encode to List[Byte](0, -128)" >> {
      encoder(Nil) must_== List[Byte](0, -128)
    }
  }

  "Encoder for -256" >> {
    val encoder = encodeFixed(-256)

    "should encode to List[Byte](-1, 0)" >> {
      encoder(Nil) must_== List[Byte](-1, 0)
    }
  }

  "Encoder for 256" >> {
    val encoder = encodeFixed(256)

    "should encode to List[Byte](1, 0)" >> {
      encoder(Nil) must_== List[Byte](1, 0)
    }
  }

  "Encoder for false" >> {
    val encoder = encodeData(false)

    "should encode to List[Byte](0)" >> {
      encoder(Nil) must_== List[Byte](0)
    }
  }

  "Encoder for true" >> {
    val encoder = encodeData(true)

    "should encode to List[Byte](-1)" >> {
      encoder(Nil) must_== List[Byte](-1)
    }
  }

  "Encoder for \"\"" >> {
    val encoder = encodeRaw("")

    "should encode to List[Byte]()" >> {
      encoder(Nil) must_== List[Byte]()
    }
  }

  "Encoder for \"abc\"" >> {
    val encoder = encodeRaw("abc")

    "should encode to List[Byte](97, 98, 99)" >> {
      encoder(Nil) must_== List[Byte](97, 98, 99)
    }
  }

  "Length encoder for 0" >> {
    val encoder = encodeLength(0)

    "should encode to List[Byte](0)" >> {
      encoder(Nil) must_== List[Byte](0)
    }
  }

  "Length encoder for 1" >> {
    val encoder = encodeLength(1)

    "should encode to List[Byte](1)" >> {
      encoder(Nil) must_== List[Byte](1)
    }
  }

  "Length encoder for 127" >> {
    val encoder = encodeLength(127)

    "should encode to List[Byte](127)" >> {
      encoder(Nil) must_== List[Byte](127)
    }
  }

  "Length encoder for 128" >> {
    val encoder = encodeLength(128)

    "should encode to List[Byte](0x81, 128)" >> {
      encoder(Nil) must_== List[Byte](0x81.toByte, 128.toByte)
    }
  }

  "Encoder for AsnTrue" >> {
    val encoder = encode(AsnTrue)

    "should encode to List[Byte](1, 1, 0xff)" >> {
      encoder(Nil) must_== List[Byte](1, 1, 0xff.toByte)
    }
  }

  "Encoder for AsnFalse" >> {
    val encoder = encode(AsnFalse)

    "should encode to List[Byte](1, 1, 0)" >> {
      encoder(Nil) must_== List[Byte](1, 1, 0)
    }
  }

  "Encoder for AsnNull" >> {
    val encoder = encode(AsnNull)

    "should encode to List[Byte](5, 0)" >> {
      encoder(Nil) must_== List[Byte](5, 0)
    }
  }
}
