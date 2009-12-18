package test.org.asn1gen.runtime.codec

import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.runtime.codec._
import _root_.java.io._
import _root_.org.asn1gen.runtime.codec._

class TestBerDecoder {
  def assertThrows[E](f: => Unit): Unit = {
    try {
      f
    } catch {
      case e: E => return
      case e => throw e
    }
    throw new Exception("Exception expected")
  }
  
  @Test
  def test_constructor_01(): Unit = {
    val data = Array[Byte](0, 1, 0)
    val is = new ByteArrayInputStream(data)
    new BerDecoder(is)
  }
  
  @Test
  def test_readTripleWindow_00(): Unit = {
    val data = Array[Byte](0, 0, 0)
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoder(is)
    val window = decoder.readTripleWindow()
    assertEquals(2, window.length)
    assertEquals(0, window.buffer(0))
    assertEquals(0, window.buffer(1))
  }
  
  
  @Test
  def test_readTripleWindow_01(): Unit = {
    val data = Array[Byte](0, 1, 0)
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoder(is)
    val window = decoder.readTripleWindow()
    assertEquals(3, window.length)
    assertEquals(0, window.buffer(0))
    assertEquals(1, window.buffer(1))
    assertEquals(0, window.buffer(2))
  }
  
  @Test
  def test_readTripleWindow_02(): Unit = {
    val data = Array[Byte](0, 1, 100, 101)
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoder(is)
    val window = decoder.readTripleWindow()
    assertEquals(3, window.length)
    assertEquals(0, window.buffer(0))
    assertEquals(1, window.buffer(1))
    assertEquals(100, window.buffer(2))
  }
  
  @Test
  def test_readTripleWindow_03(): Unit = {
    val data = Array[Byte](0, 2, 100, 101)
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoder(is)
    val window = decoder.readTripleWindow()
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
    val decoder = new BerDecoder(is)
    val window = decoder.readTripleWindow()
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
    val decoder = new BerDecoder(is)
    val window = decoder.readTripleWindow()
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
    println(data(1))
    val is = new ByteArrayInputStream(data)
    val decoder = new BerDecoder(is)
    val window = decoder.readTripleWindow()
    assertEquals(6, window.length)
    assertEquals(31, window.buffer(0))
    assertEquals(0x80.toByte, window.buffer(1))
    assertEquals(0, window.buffer(2))
    assertEquals(2, window.buffer(3))
    assertEquals(100, window.buffer(4))
    assertEquals(101, window.buffer(5))
  }
}
