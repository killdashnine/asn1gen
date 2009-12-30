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
  object TheDecoder extends PackratBerDecoder {
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
}
