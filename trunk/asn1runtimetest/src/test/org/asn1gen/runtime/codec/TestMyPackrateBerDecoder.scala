package test.org.asn1gen.runtime.codec

import org.asn1gen.runtime._
import org.asn1gen.runtime.codec._
import org.asn1gen.runtime.codec.async._
import org.junit._
import org.junit.Assert._
import org.asn1gen.junit.Assert._
import test.asn1.genruntime.BerDecoder
import test.asn1.genruntime.ExemplarDecoder
import test.asn1.genruntime.ExemplarRealiser
import org.asn1gen.parsing.ByteReader

class TestMyPackratBerDecoder {
  object TheDecoder extends ExemplarDecoder with ExemplarRealiser {
    def parse[N](root: Parser[N], input: Array[Byte]) =
      phrase(root)(new ByteReader(input))
  }
  
  import TheDecoder._
  
  @Test
  def test_mySequence_00(): Unit = {
    val data = Array[Byte](
      0x80.toByte, 1, 42,
      0x81.toByte, 4, 0x83.toByte, 1, 1, 1,
      0x82.toByte, 3, 'a'.toByte, 'b'.toByte, 'c'.toByte,
      0x83.toByte, 2, 0x80.toByte, 0
    )
    parse(mySequence(data.length), data) match {
      case Success(result, _) => assertEquals((Some(42), 2.0, "abc", test.asn1.genruntime.MyChoice_Choice0(AsnNull)), result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_mySequence_01(): Unit = {
    val data = Array[Byte](
      0x80.toByte, 1, 42,
      0x81.toByte, 4, 0x83.toByte, 1, 1, 1,
      0x82.toByte, 3, 'a'.toByte, 'b'.toByte, 'c'.toByte,
      0x83.toByte, 3, 0x81.toByte, 1, 43
    )
    parse(mySequence(data.length), data) match {
      case Success(result, _) =>
        assertEquals(
            (Some(42), 2.0, "abc", test.asn1.genruntime.MyChoice_Choice1(AsnInteger(43))),
            result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_mySequence_02(): Unit = {
    val data = Array[Byte](
      0x80.toByte, 1, 42,
      0x81.toByte, 4, 0x83.toByte, 1, 1, 1,
      0x82.toByte, 3, 'a'.toByte, 'b'.toByte, 'c'.toByte,
      0x83.toByte, 5, 0x82.toByte, 3, 0x80.toByte, 2, 1
    )
    parse(mySequence(data.length), data) match {
      case Success(result, _) =>
        assertEquals(
            (Some(42), 2.0, "abc", test.asn1.genruntime.MyChoice_Choice2(AsnReal(4.0))),
            result)
      case x => fail("Parse failure: " + x)
    }
  }
  
  @Test
  def test_mySequence_03(): Unit = {
    val data = Array[Byte](
      0x81.toByte, 4, 0x83.toByte, 1, 1, 1,
      0x82.toByte, 3, 'a'.toByte, 'b'.toByte, 'c'.toByte,
      0x83.toByte, 5, 0x82.toByte, 3, 0x80.toByte, 2, 1
    )
    parse(mySequence(data.length), data) match {
      case Success(result, _) =>
        assertEquals(
            (None, 2.0, "abc", test.asn1.genruntime.MyChoice_Choice2(AsnReal(4.0))),
            result)
      case x => fail("Parse failure: " + x)
    }
  }
}
