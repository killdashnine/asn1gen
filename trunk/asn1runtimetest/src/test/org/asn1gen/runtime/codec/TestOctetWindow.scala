package test.org.asn1gen.runtime.codec

import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.runtime.codec._

class TestOctetWindow {
  @Test
  def testConstructor_01() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), 0, 3)
  }
  
  @Test
  def testConstructor_02() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), 0, 2)
  }
  
  @Test
  def testConstructor_03() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), 1, 2)
  }
  
  @Test
  def testConstructor_04() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), 1, 1)
  }
  
  @Test
  def testConstructor_05() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), 0, 0)
  }
  
  @Test
  def testConstructor_06() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), 1, 0)
  }
  
  @Test
  def testConstructor_07() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), 2, 0)
  }
  
  @Test
  def testConstructor_08() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), 3, 0)
  }
  
  @Test(expected = classOf[AssertionError])
  def testConstructor_09x() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), -1, 3)
  }
  
  @Test(expected = classOf[AssertionError])
  def testConstructor_10x() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), 1, 3)
  }
  
  @Test(expected = classOf[AssertionError])
  def testConstructor_11x() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), 0, -1)
  }
  
  @Test(expected = classOf[AssertionError])
  def testConstructor_12x() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), 4, 0)
  }
  
  @Test(expected = classOf[AssertionError])
  def testConstructor_13x() = {
    val window = OctetWindow(Array[Byte](1, 2, 3), -1, 0)
  }
}
