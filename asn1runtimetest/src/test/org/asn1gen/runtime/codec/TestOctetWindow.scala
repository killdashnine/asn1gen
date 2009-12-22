package test.org.asn1gen.runtime.codec

import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.runtime.codec._

class TestOctetWindow extends org.asn1gen.junit.Assert {
  @Test
  def test_constructor_01(): Unit = {
    OctetWindow(Array[Byte](1, 2, 3), 0, 3)
  }
  
  @Test
  def test_constructor_02(): Unit = {
    OctetWindow(Array[Byte](1, 2, 3), 0, 2)
  }
  
  @Test
  def test_constructor_03(): Unit = {
    OctetWindow(Array[Byte](1, 2, 3), 1, 2)
  }
  
  @Test
  def test_constructor_04(): Unit = {
    OctetWindow(Array[Byte](1, 2, 3), 1, 1)
  }
  
  @Test
  def test_constructor_05(): Unit = {
    OctetWindow(Array[Byte](1, 2, 3), 0, 0)
  }
  
  @Test
  def test_constructor_06(): Unit = {
    OctetWindow(Array[Byte](1, 2, 3), 1, 0)
  }
  
  @Test
  def test_constructor_07(): Unit = {
    OctetWindow(Array[Byte](1, 2, 3), 2, 0)
  }
  
  @Test
  def test_constructor_08(): Unit = {
    OctetWindow(Array[Byte](1, 2, 3), 3, 0)
  }
  
  @Test
  def test_constructor_09x(): Unit = {
    assertThrows[AssertionError] { OctetWindow(Array[Byte](1, 2, 3), -1, 3) }
  }
  
  @Test
  def test_constructor_10x(): Unit = {
    assertThrows[AssertionError] { OctetWindow(Array[Byte](1, 2, 3), 1, 3) }
  }
  
  @Test
  def test_constructor_11x(): Unit = {
    assertThrows[AssertionError] { OctetWindow(Array[Byte](1, 2, 3), 0, -1) }
  }
  
  @Test
  def test_constructor_12x(): Unit = {
    assertThrows[AssertionError] { OctetWindow(Array[Byte](1, 2, 3), 4, 0) }
  }
  
  @Test
  def test_constructor_13x(): Unit = {
    assertThrows[AssertionError] { OctetWindow(Array[Byte](1, 2, 3), -1, 0) }
  }
  
  @Test
  def test_end_1(): Unit = {
    val window = OctetWindow(Array[Byte](1, 2, 3), 0, 3)
    assert(window.end == 3)
  }
  
  @Test
  def test_contains_1(): Unit = {
    val data = Array[Byte](1, 2, 3)
    val window1 = OctetWindow(data, 0, 3)
    val window2 = OctetWindow(data, 0, 3)
    assert(window1 contains window2)
  }
  
  @Test
  def test_contains_2(): Unit = {
    val data = Array[Byte](1, 2, 3)
    val window1 = OctetWindow(data, 0, 3)
    val window2 = OctetWindow(data, 1, 2)
    assert(window1 contains window2)
  }
  
  @Test
  def test_contains_3(): Unit = {
    val data = Array[Byte](1, 2, 3)
    val window1 = OctetWindow(data, 0, 3)
    val window2 = OctetWindow(data, 0, 2)
    assert(window1 contains window2)
  }
  
  @Test
  def test_contains_4(): Unit = {
    val data = Array[Byte](1, 2, 3)
    val window1 = OctetWindow(data, 0, 3)
    val window2 = OctetWindow(data, 0, 0)
    assert(window1 contains window2)
  }
  
  @Test
  def test_contains_5(): Unit = {
    val data = Array[Byte](1, 2, 3)
    val window1 = OctetWindow(data, 0, 3)
    val window2 = OctetWindow(data, 3, 0)
    assert(window1 contains window2)
  }
  
  @Test
  def test_contains_6(): Unit = {
    val data = Array[Byte](1, 2, 3)
    val window1 = OctetWindow(data, 1, 2)
    val window2 = OctetWindow(data, 1, 2)
    assert(window1 contains window2)
  }
  
  @Test
  def test_contains_7(): Unit = {
    val data = Array[Byte](1, 2, 3)
    val window1 = OctetWindow(data, 1, 2)
    val window2 = OctetWindow(data, 0, 2)
    assert(!window1.contains(window2))
  }
  
  @Test
  def test_contains_8(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 1, 2)
    val window2 = OctetWindow(data, 1, 3)
    assert(!window1.contains(window2))
  }
  
  @Test
  def test_contains_9(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 1, 2)
    val window2 = OctetWindow(data, 0, 4)
    assert(!window1.contains(window2))
  }
  
  @Test
  def test_zoomAbsolute_1(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 0, 4)
    val window2 = window1.zoomAbsolute(0, 2)
    assert(window2.start == 0 && window2.end == 2)
  }
  
  @Test
  def test_zoomAbsolute_2(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 0, 4)
    val window2 = window1.zoomAbsolute(2, 2)
    assert(window2.start == 2 && window2.end == 4)
  }
  
  @Test
  def test_zoomAbsolute_3x(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 0, 4)
    val window2 = window1.zoomAbsolute(0, 2)
    assertThrows[AssertionError] { window2.zoomAbsolute(2, 2) }
  }
  
  @Test
  def test_zoomAbsolute_4x(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 0, 4)
    val window2 = window1.zoomAbsolute(2, 2)
    assertThrows[AssertionError] { window2.zoomAbsolute(0, 2) }
  }
  
  @Test
  def test_zoom_1(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 0, 4)
    val window2 = window1.zoom(0, 2)
    assert(window2.start == 0 && window2.end == 2)
  }
  
  @Test
  def test_zoom_2(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 0, 4)
    val window2 = window1.zoom(2, 2)
    assert(window2.start == 2 && window2.end == 4)
  }
  
  @Test
  def test_zoom_3x(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 0, 4)
    val window2 = window1.zoom(0, 2)
    assertThrows[AssertionError] { window2.zoom(2, 2) }
  }
  
  @Test
  def test_zoom_4(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 0, 4)
    val window2 = window1.zoom(2, 2)
    val window3 = window2.zoom(0, 2)
    assertEquals(2, window3.start)
    assertEquals(4, window3.end)
  }
  
  @Test
  def test_zoom_5(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 0, 4)
    val window2 = window1.zoom(2, 2)
    val window3 = window2.zoom(0, 0)
    assertEquals(2, window3.start)
    assertEquals(2, window3.end)
  }
  
  @Test
  def test_from_1(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 0, 4)
    val window2 = window1.from(1)
    assertEquals(1, window2.start)
    assertEquals(4, window2.end)
    val window3 = window2.from(1)
    assertEquals(2, window3.start)
    assertEquals(4, window3.end)
    val window4 = window3.from(1)
    assertEquals(3, window4.start)
    assertEquals(4, window4.end)
    val window5 = window4.from(1)
    assertEquals(4, window5.start)
    assertEquals(4, window5.end)
  }
  
  @Test
  def test_from_3x(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 4, 0)
    assertThrows[AssertionError] { window1.from(1) }
  }
  
  @Test
  def test_from_4x(): Unit = {
    val data = Array[Byte](1, 2, 3, 4)
    val window1 = OctetWindow(data, 3, 0)
    assertThrows[AssertionError] { window1.from(1) }
  }
}
