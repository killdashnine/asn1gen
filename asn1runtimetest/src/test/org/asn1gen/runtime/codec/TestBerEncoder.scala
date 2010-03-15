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

class TestBerEncoder {
  @Test
  def fixedInteger_1(): Unit = {
    val result = encodeFixed(0)
    val expected = List[Byte]()
    assertEquals(expected, result(Nil))
    assertEquals(expected.length, result.length)
  }

  @Test
  def fixedInteger_2(): Unit = {
    val result = encodeFixed(-1)
    val expected = List[Byte](-1)
    assertEquals(expected, result(Nil))
    assertEquals(expected.length, result.length)
  }

  @Test
  def fixedInteger_3(): Unit = {
    val result = encodeFixed(42)
    val expected = List[Byte](42)
    assertEquals(expected, result(Nil))
    assertEquals(expected.length, result.length)
  }

  @Test
  def fixedInteger_4(): Unit = {
    val result = encodeFixed(-42)
    val expected = List[Byte](-42)
    assertEquals(expected, result(Nil))
    assertEquals(expected.length, result.length)
  }

  @Test
  def fixedInteger_5(): Unit = {
    val result = encodeFixed(255)
    val expected = List[Byte](0, -1)
    assertEquals(expected, result(Nil))
    assertEquals(expected.length, result.length)
  }

  @Test
  def fixedInteger_6(): Unit = {
    val result = encodeFixed(128)
    val expected = List[Byte](0, -128)
    assertEquals(expected, result(Nil))
    assertEquals(expected.length, result.length)
  }
}
