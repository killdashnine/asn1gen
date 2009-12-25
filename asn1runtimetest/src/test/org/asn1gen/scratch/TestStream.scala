package test.org.asn1gen.scratch

import java.io._
import org.junit._
import org.junit.Assert._

class TestStream {
  @Test
  def test_scalaStream_01(): Unit = {
    val data = Array[Byte](31, 0x80.toByte, 0, 3, 100, 101)
    val is = new ByteArrayInputStream(data)
    val stream = Stream.continually(is.read).takeWhile(_ != -1).map(_.toByte)
  }
}
