package test.org.asn1gen.runtime.codec

import org.asn1gen.runtime._
import org.asn1gen.runtime.codec._
import org.asn1gen.runtime.codec.async._
import org.junit._
import org.junit.Assert._
import org.asn1gen.junit.Assert._
import test.asn1.genruntime.BerDecoder
import test.asn1.genruntime.MyPackratBerDecoder
import test.asn1.genruntime.MyPackratBerRealiser
import org.asn1gen.parsing.ByteReader

class TestMyPackratBerDecoder {
  object TheDecoder extends MyPackratBerDecoder with MyPackratBerRealiser {
    def parse[N](root: Parser[N], input: Array[Byte]) =
      phrase(root)(new ByteReader(input))
  }
  
  import TheDecoder._
  
  @Test
  def test_tlLength_00(): Unit = {
    val data = Array[Byte](0)
    parse(tlLength, data) match {
      case Success(length, _) =>
        assertEquals(0, length)
      case x => fail("Parse failure: " + x)
    }
  }

  
  @Test
  def test_mySequence_00(): Unit = {
    // header     exponent   number
    // 1000 0000  0000 0000  0000 0001
    val data = Array[Byte](
      0x80.toByte, 1, 42,
      0x81.toByte, 1, 43
    )
    parse(mySequence(data.length), data) match {
      case Success(result, _) => assertEquals((42, 43), result)
      case x => fail("Parse failure: " + x)
    }
  }
}
