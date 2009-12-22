package org.asn1gen.extra.test

import org.junit._
import org.junit.Assert._
import junit.framework.TestCase
import org.asn1gen.extra._

class TestByteExtra extends Extras {
  @Test
  def test_definesBit_00(): Unit = {
    assert(!0x0.toByte.definesBit(0))
  }

  @Test
  def test_definesBit_01(): Unit = {
    assert(0x1.toByte.definesBit(0))
  }

  @Test
  def test_definesBit_02(): Unit = {
    assert(!0x2.toByte.definesBit(0))
  }

  @Test
  def test_definesBit_03(): Unit = {
    assert(0x2.toByte.definesBit(1))
  }

  @Test
  def test_definesBit_04(): Unit = {
    assert(0x3.toByte.definesBit(1))
  }

  @Test
  def test_definesBit_05(): Unit = {
    assert(!0x4.toByte.definesBit(1))
  }
}
