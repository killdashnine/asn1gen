package test.org.asn1gen.runtime.codec

import org.asn1gen.runtime._
import org.asn1gen.runtime.codec._
import org.asn1gen.runtime.codec.async._
import org.junit._
import org.junit.Assert._
import org.asn1gen.junit.Assert._
import test.asn1.genruntime.BerDecoder
import org.asn1gen.parsing.ByteReader

class TestPackratBerDecoder {
  object TheDecoder extends PackratBerDecoder with PackratBerRealiser {
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
  def test_tlLength_01(): Unit = {
    val data = Array[Byte](1)
    parse(tlLength, data) match {
      case Success(length, _) =>
        assertEquals(1, length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlLength_02(): Unit = {
    val data = Array[Byte](2)
    parse(tlLength, data) match {
      case Success(length, _) =>
        assertEquals(2, length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlLength_03(): Unit = {
    val data = Array[Byte](0xff.toByte, 0x7f)
    parse(tlLength, data) match {
      case Success(length, _) =>
        assertEquals(16383, length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlLength_04(): Unit = {
    val data = Array[Byte](0xd5.toByte, 0xd5.toByte, 0x55.toByte)
    parse(tlLength, data) match {
      case Success(length, _) =>
        assertEquals(1403605, length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlLength_05(): Unit = {
    val data = Array[Byte](0xd5.toByte, 0xd5.toByte, 0x54.toByte)
    parse(tlLength, data) match {
      case Success(length, _) =>
        assertEquals(1403604, length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlLength_06(): Unit = {
    val data = Array[Byte](0xd5.toByte, 0xd4.toByte, 0x55.toByte)
    parse(tlLength, data) match {
      case Success(length, _) =>
        assertEquals(1403477, length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlLength_07(): Unit = {
    val data = Array[Byte](0xd4.toByte, 0xd5.toByte, 0x55.toByte)
    parse(tlLength, data) match {
      case Success(length, _) =>
        assertEquals(1387221, length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlTag_00(): Unit = {
    val data = Array[Byte](0)
    parse(tlTag, data) match {
      case Success(triplet, _) =>
        assertEquals(TagClass.Universal, triplet.tagClass)
        assertEquals(false, triplet.constructed)
        assertEquals(0, triplet.tagType)
        assertEquals(0, triplet.length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlTag_01(): Unit = {
    val data = Array[Byte](0x40)
    parse(tlTag, data) match {
      case Success(triplet, _) =>
        assertEquals(TagClass.Application, triplet.tagClass)
        assertEquals(false, triplet.constructed)
        assertEquals(0, triplet.tagType)
        assertEquals(0, triplet.length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlTag_02(): Unit = {
    val data = Array[Byte](0x80.toByte)
    parse(tlTag, data) match {
      case Success(triplet, _) =>
        assertEquals(TagClass.ContextSpecific, triplet.tagClass)
        assertEquals(false, triplet.constructed)
        assertEquals(0, triplet.tagType)
        assertEquals(0, triplet.length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlTag_03(): Unit = {
    val data = Array[Byte](0xc0.toByte)
    parse(tlTag, data) match {
      case Success(triplet, _) =>
        assertEquals(TagClass.Private, triplet.tagClass)
        assertEquals(false, triplet.constructed)
        assertEquals(0, triplet.tagType)
        assertEquals(0, triplet.length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlTag_04(): Unit = {
    val data = Array[Byte](0x20.toByte)
    parse(tlTag, data) match {
      case Success(triplet, _) =>
        assertEquals(TagClass.Universal, triplet.tagClass)
        assertEquals(true, triplet.constructed)
        assertEquals(0, triplet.tagType)
        assertEquals(0, triplet.length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlTag_05(): Unit = {
    val data = Array[Byte](24)
    parse(tlTag, data) match {
      case Success(triplet, _) =>
        assertEquals(TagClass.Universal, triplet.tagClass)
        assertEquals(false, triplet.constructed)
        assertEquals(24, triplet.tagType)
        assertEquals(0, triplet.length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlTag_06(): Unit = {
    val data = Array[Byte](31, 42)
    parse(tlTag, data) match {
      case Success(triplet, _) =>
        assertEquals(TagClass.Universal, triplet.tagClass)
        assertEquals(false, triplet.constructed)
        assertEquals(42, triplet.tagType)
        assertEquals(0, triplet.length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlTag_07(): Unit = {
    val data = Array[Byte](31, 0x80.toByte, 42)
    parse(tlTag, data) match {
      case Success(triplet, _) =>
        assertEquals(TagClass.Universal, triplet.tagClass)
        assertEquals(false, triplet.constructed)
        assertEquals(42, triplet.tagType)
        assertEquals(0, triplet.length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlTag_08(): Unit = {
    val data = Array[Byte](31, 0x81.toByte, 0x7f)
    parse(tlTag, data) match {
      case Success(triplet, _) =>
        assertEquals(TagClass.Universal, triplet.tagClass)
        assertEquals(false, triplet.constructed)
        assertEquals(255, triplet.tagType)
        assertEquals(0, triplet.length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_tlTag_09(): Unit = {
    val data = Array[Byte](31, 0xd4.toByte, 0xd5.toByte, 0x55.toByte)
    parse(tlTag, data) match {
      case Success(triplet, _) =>
        assertEquals(TagClass.Universal, triplet.tagClass)
        assertEquals(false, triplet.constructed)
        assertEquals(1387221, triplet.tagType)
        assertEquals(0, triplet.length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  
  @Test
  def test_tl_01(): Unit = {
    val data = Array[Byte](0, 1)
    parse(tl, data) match {
      case Success(triplet, _) =>
        assertEquals(TagClass.Universal, triplet.tagClass)
        assertEquals(false, triplet.constructed)
        assertEquals(0, triplet.tagType)
        assertEquals(1, triplet.length)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnNull_00(): Unit = {
    val data = Array[Byte](0)
    parse(length >> asnNull, data) match {
      case Success(result, _) => assertEquals((), result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnNull_01(): Unit = {
    val data = Array[Byte](1)
    assertThrows(classOf[DecodingException]) {
      parse(length >> asnNull, data) match {
        case Success(result, _) => assertEquals((), result)
        case x => fail("Parse failure: " + x)
      }
    }
  }
  
  @Test
  def test_asnBoolean_00(): Unit = {
    val data = Array[Byte](0)
    assertThrows(classOf[DecodingException]) {
      parse(length >> asnBoolean, data) match {
        case Success(result, _) => assertEquals((), result)
        case x => fail("Parse failure: " + x)
      }
    }
  }
  
  @Test
  def test_asnBoolean_01(): Unit = {
    val data = Array[Byte](1)
    assertThrows(classOf[DecodingException]) {
      parse(length >> asnBoolean, data) match {
        case Success(result, _) => assertEquals((), result)
        case x => throw new DecodingException("EOF unexpected")
      }
    }
  }
  
  @Test
  def test_asnBoolean_02(): Unit = {
    val data = Array[Byte](2)
    assertThrows(classOf[DecodingException]) {
      parse(length >> asnBoolean, data) match {
        case Success(result, _) => assertEquals((), result)
        case x => fail("Parse failure: " + x)
      }
    }
  }
  
  @Test
  def test_asnBoolean_03(): Unit = {
    val data = Array[Byte](1, 0)
    parse(length >> asnBoolean, data) match {
      case Success(result, _) => assertEquals(false, result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnBoolean_04(): Unit = {
    val data = Array[Byte](1, 1)
    parse(length >> asnBoolean, data) match {
      case Success(result, _) => assertEquals(true, result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnBoolean_05(): Unit = {
    val data = Array[Byte](1, 8)
    parse(length >> asnBoolean, data) match {
      case Success(result, _) => assertEquals(true, result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnBoolean_06(): Unit = {
    val data = Array[Byte](1, 0xff.toByte)
    parse(length >> asnBoolean, data) match {
      case Success(result, _) => assertEquals(true, result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnBoolean_07(): Unit = {
    val data = Array[Byte](2)
    assertThrows(classOf[DecodingException]) {
      parse(length >> asnBoolean, data) match {
        case Success(result, _) => assertEquals((), result)
        case x => fail("Parse failure: " + x)
      }
    }
  }
  
  @Test
  def test_asnBoolean_08(): Unit = {
    val data = Array[Byte](2, 0, 0)
    assertThrows(classOf[DecodingException]) {
      parse(length >> asnBoolean, data) match {
        case Success(result, _) => assertEquals((), result)
        case x => fail("Parse failure: " + x)
      }
    }
  }
  
  @Test
  def test_asnInteger_00(): Unit = {
    val data = Array[Byte](100.toByte)
    parse(asnInteger(1), data) match {
      case Success(result, _) => assertEquals(100, result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnInteger_01(): Unit = {
    val data = Array[Byte](156.toByte)
    parse(asnInteger(1), data) match {
      case Success(result, _) => assertEquals(-100, result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnInteger_02(): Unit = {
    val data = Array[Byte](105.toByte, 186.toByte)
    parse(asnInteger(2), data) match {
      // 0110 1001 1011 1010
      case Success(result, _) => assertEquals(27066, result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnInteger_03(): Unit = {
    val data = Array[Byte](150.toByte, 70.toByte)
    parse(asnInteger(2), data) match {
      case Success(result, _) => assertEquals(-27066, result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnInteger_04(): Unit = {
    val data = Array[Byte](8.toByte, 4.toByte, 2.toByte, 1.toByte)
    parse(asnInteger(4), data) match {
      case Success(result, _) => assertEquals(134480385, result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnInteger_05(): Unit = {
    // 1000 0000 0100 0000 0010 0000 0010 0000
    val data = Array[Byte](127.toByte, 191.toByte, 223.toByte, 240.toByte)
    parse(asnInteger(4), data) match {
      case Success(result, _) => assertEquals(2143281136, result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnInteger_06(): Unit = {
    // 1000 0000 0100 0000 0010 0000 0010 0000
    val data = Array[Byte](127.toByte, 191.toByte, 223.toByte, 240.toByte)
    parse(asnInteger(3), data) match {
      case Failure("end of input expected", _) => ()
      case _ => fail("Error expected")
    }
  }
  
  @Test
  def test_asnInteger_07(): Unit = {
    // 1000 0000 0100 0000 0010 0000 0010 0000
    val data = Array[Byte](127.toByte, 191.toByte, 223.toByte, 240.toByte)
    parse(asnInteger(5), data) match {
      case Failure("EOF unexpected", y) => ()
      case _ => fail("Error expected")
    }
  }
  
  @Test
  def test_asnReal_00(): Unit = {
    val data = Array[Byte]()
    parse(asnReal(0), data) match {
      case Success(result, _) => assertEquals(0.0, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_01(): Unit = {
    val data = Array[Byte](64.toByte)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(Double.PositiveInfinity, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_02(): Unit = {
    val data = Array[Byte](65.toByte)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(Double.NegativeInfinity, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_03(): Unit = {
    val data = Array.concat(Array[Byte](1), "3".map{c => c.toByte}.toArray)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(3.0, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_04(): Unit = {
    val data = Array.concat(Array[Byte](1), "-1".map{c => c.toByte}.toArray)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(-1.0, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_05(): Unit = {
    val data = Array.concat(Array[Byte](1), "+1000".map{c => c.toByte}.toArray)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(1000.0, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_06(): Unit = {
    val data = Array.concat(Array[Byte](2), "3.0".map{c => c.toByte}.toArray)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(3.0, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_07(): Unit = {
    val data = Array.concat(Array[Byte](2), "-1.3".map{c => c.toByte}.toArray)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(-1.3, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_08(): Unit = {
    val data = Array.concat(Array[Byte](2), "-.3".map{c => c.toByte}.toArray)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(-0.3, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_09(): Unit = {
    val data = Array.concat(Array[Byte](2), "3.0E1".map{c => c.toByte}.toArray)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(30.0, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_10(): Unit = {
    val data = Array.concat(Array[Byte](2), "123E+100".map{c => c.toByte}.toArray)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(123e+100, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_11(): Unit = {
    // header     exponent   number
    // 1000 0000  0000 0000  0000 0001
    val data = Array[Byte](0x80.toByte, 0, 1)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(1.0, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_12(): Unit = {
    // header     exponent   number
    // 1000 0000  0000 0000  0000 0001
    val data = Array[Byte](0x80.toByte, 1, 1)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(2.0, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_13(): Unit = {
    // header     exponent   number
    // 1000 0000  0000 0000  0000 0001
    val data = Array[Byte](0x80.toByte, 2, 1)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(4.0, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_15(): Unit = {
    // header     exponent   number
    // 1000 0000  0000 0000  0000 0001
    val data = Array[Byte](0x83.toByte, 1, 0, 1)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(1.0, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_16(): Unit = {
    // header     exponent   number
    // 1000 0000  0000 0000  0000 0001
    val data = Array[Byte](0x83.toByte, 1, 1, 1)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(2.0, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnReal_17(): Unit = {
    // header     exponent   number
    // 1000 0000  0000 0000  0000 0001
    val data = Array[Byte](0x83.toByte, 1, 2, 1)
    parse(asnReal(data.length), data) match {
      case Success(result, _) => assertEquals(4.0, result, 0.0)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_asnOctetString_00(): Unit = {
    // header     exponent   number
    // 1000 0000  0000 0000  0000 0001
    val data = Array[Byte]('a'.toByte, 'b'.toByte, 'c'.toByte)
    parse(asnOctetString(data.length), data) match {
      case Success(result, _) => assertEquals(List('a'.toByte, 'b'.toByte, 'c'.toByte), result)
      case x => fail("Parse failure: " + x)
    }
  }
}
