package test.org.asn1gen.runtime.codec

import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.runtime._
import _root_.org.asn1gen.runtime.codec._
import _root_.java.io._

class TestBerDecoder extends org.asn1gen.junit.Assert {
  @Test
  def test_decodeTriplet_01(): Unit = {
    val data = Array[Byte](0, 1, 0)
    val is = new DecodingInputStream(new ByteArrayInputStream(data))

    val decoder = new BerDecoder
    
    val triplet = decoder.decodeTriplet(is)
    assertEquals(0, triplet.tagType)
    assertEquals(1, triplet.length)
    is.skip(triplet.length)
    
    assertEquals(0, triplet.tagType)
    assertEquals(1, triplet.length)
  }
  
  @Test
  def test_decode_AsnBoolean_00(): Unit = {
    val data = Array[Byte](1, 1, 0)
    val is = new DecodingInputStream(new ByteArrayInputStream(data))

    val decoder = new BerDecoder
    
    val value = decoder.decode(is, AsnBoolean)
    
    assertEquals(AsnBoolean(false), value)
  }
  
  @Test
  def test_decode_AsnBoolean_01(): Unit = {
    val data = Array[Byte](1, 1, 0xff.toByte)
    val is = new DecodingInputStream(new ByteArrayInputStream(data))
    val decoder = new BerDecoder
    val value = decoder.decode(is, AsnBoolean)
    
    assertEquals(AsnBoolean(true), value)
  }
  
  @Test
  def test_decode_AsnBoolean_02(): Unit = {
    val data = Array[Byte](1, 1, 1)
    val is = new DecodingInputStream(new ByteArrayInputStream(data))
    val decoder = new BerDecoder
    val value = decoder.decode(is, AsnBoolean)
    
    assertEquals(AsnBoolean(true), value)
  }
  
  @Test
  def test_constructor_01(): Unit = {
    val data = Array[Byte](0, 1, 0)
    val is = new ByteArrayInputStream(data)
    new BerDecoderReader(is)
  }
  
  @Test
  def test_readTripleWindow_00(): Unit = {
    val data = Array[Byte](0, 0, 0)
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoderReader(is)
    val window = decoder.readTripletWindow()
    assertEquals(2, window.length)
    assertEquals(0, window.buffer(0))
    assertEquals(0, window.buffer(1))
  }
  
  def bb(value :Int): Byte = {
    assert(value >= 0)
    if (value == 0) {
      return 0
    } else {
      val digit = value % 10
      assert(digit < 2)
      ((bb(value / 10) << 1) | digit).toByte
    }
  }
  
  @Test
  def test_decode_AsnNull_00(): Unit = {
    val data = Array[Byte](5, 0)
    val is = new DecodingInputStream(new ByteArrayInputStream(data))
    val decoder = new BerDecoder
    val value = decoder.decode(is, AsnNull)
    assertEquals(AsnNull, value)
  }
  
  @Test
  def test_decode_AsnInteger_00(): Unit = {
    val data = Array[Byte](2, 2, bb(10010110), bb(1000110))
    val is = new DecodingInputStream(new ByteArrayInputStream(data))
    val decoder = new BerDecoder
    val value = decoder.decode(is, AsnInteger)
    println(value)
    assertEquals(AsnInteger(-27066), value)
  }
  
  
  @Test
  def test_readTripleWindow_01(): Unit = {
    val data = Array[Byte](0, 1, 0)
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoderReader(is)
    val window = decoder.readTripletWindow()
    assertEquals(3, window.length)
    assertEquals(0, window.buffer(0))
    assertEquals(1, window.buffer(1))
    assertEquals(0, window.buffer(2))
  }
  
  @Test
  def test_readTripleWindow_02(): Unit = {
    val data = Array[Byte](0, 1, 100, 101)
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoderReader(is)
    val window = decoder.readTripletWindow()
    assertEquals(3, window.length)
    assertEquals(0, window.buffer(0))
    assertEquals(1, window.buffer(1))
    assertEquals(100, window.buffer(2))
  }
  
  @Test
  def test_readTripleWindow_03(): Unit = {
    val data = Array[Byte](0, 2, 100, 101)
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoderReader(is)
    val window = decoder.readTripletWindow()
    assertEquals(4, window.length)
    assertEquals(0, window.buffer(0))
    assertEquals(2, window.buffer(1))
    assertEquals(100, window.buffer(2))
    assertEquals(101, window.buffer(3))
  }
  
  @Test
  def test_readTripleWindow_04(): Unit = {
    val data = Array[Byte](30, 2, 100, 101)
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoderReader(is)
    val window = decoder.readTripletWindow()
    assertEquals(4, window.length)
    assertEquals(30, window.buffer(0))
    assertEquals(2, window.buffer(1))
    assertEquals(100, window.buffer(2))
    assertEquals(101, window.buffer(3))
  }
  
  @Test
  def test_readTripleWindow_05(): Unit = {
    val data = Array[Byte](31, 0, 2, 100, 101)
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoderReader(is)
    val window = decoder.readTripletWindow()
    assertEquals(5, window.length)
    assertEquals(31, window.buffer(0))
    assertEquals(0, window.buffer(1))
    assertEquals(2, window.buffer(2))
    assertEquals(100, window.buffer(3))
    assertEquals(101, window.buffer(4))
  }
  
  @Test
  def test_readTripleWindow_06(): Unit = {
    val data = Array[Byte](31, 0x80.toByte, 0, 2, 100, 101)
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoderReader(is)
    val window = decoder.readTripletWindow()
    assertEquals(6, window.length)
    assertEquals(31, window.buffer(0))
    assertEquals(0x80.toByte, window.buffer(1))
    assertEquals(0, window.buffer(2))
    assertEquals(2, window.buffer(3))
    assertEquals(100, window.buffer(4))
    assertEquals(101, window.buffer(5))
  }
  
  @Test
  def test_readTripleWindow_07(): Unit = {
    val data = Array[Byte](31, 0x80.toByte, 0, 3, 100, 101)
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoderReader(is)
    assertThrows[IndexOutOfBoundsException] {
      val window = decoder.readTripletWindow()
    }
  }
  
  @Test
  def test_scalaStream_01(): Unit = {
    val data = Array[Byte](31, 0x80.toByte, 0, 3, 100, 101)
    val is = new ByteArrayInputStream(data)
    val stream = Stream.continually(is.read).takeWhile(_ != -1).map(_.toByte) 
    stream.print
  }
}
