package test.org.asn1gen.extra

import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.org.asn1gen.extra._

import org.junit.runner.RunWith;
import org.junit.runners.BlockJUnit4ClassRunner;

@RunWith(classOf[BlockJUnit4ClassRunner])
class TestExtraString extends Extras {
  @Test(expected = classOf[IllegalArgumentException])
  def test_none(): Unit = "".bin
  
  @Test def test_0(): Unit = assertEquals(0, "0".bin)
  @Test def test_1(): Unit = assertEquals(1, "1".bin)
  
  @Test def test_00(): Unit = assertEquals(0, "00".bin)
  @Test def test_01(): Unit = assertEquals(1, "01".bin)
  @Test def test_10(): Unit = assertEquals(2, "10".bin)
  @Test def test_11(): Unit = assertEquals(3, "11".bin)
  
  @Test def test_000(): Unit = assertEquals(0, "000".bin)
  @Test def test_001(): Unit = assertEquals(1, "001".bin)
  @Test def test_010(): Unit = assertEquals(2, "010".bin)
  @Test def test_011(): Unit = assertEquals(3, "011".bin)
  @Test def test_100(): Unit = assertEquals(4, "100".bin)
  @Test def test_101(): Unit = assertEquals(5, "101".bin)
  @Test def test_110(): Unit = assertEquals(6, "110".bin)
  @Test def test_111(): Unit = assertEquals(7, "111".bin)
}
