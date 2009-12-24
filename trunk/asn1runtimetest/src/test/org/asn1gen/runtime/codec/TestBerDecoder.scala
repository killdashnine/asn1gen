package test.org.asn1gen.runtime.codec

import org.asn1gen.runtime._
import org.asn1gen.runtime.codec._
import java.io._
import org.junit._
import org.junit.Assert._
import org.asn1gen.junit.Assert._
import test.asn1.genruntime.BerDecoder

class TestBerDecoder {
  @Test
  def test_decodeTriplet_01(): Unit = {
    val data = Array[Byte](0, 1, 0)
    val is = new DecodingInputStream(new ByteArrayInputStream(data))

    val triplet = BerDecoder.decodeTriplet(is)
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

    val value = BerDecoder.decode(is, AsnBoolean)
    
    assertEquals(AsnBoolean(false), value)
  }
  
  @Test
  def test_decode_AsnBoolean_01(): Unit = {
    val data = Array[Byte](1, 1, 0xff.toByte)
    val is = new DecodingInputStream(new ByteArrayInputStream(data))
    val value = BerDecoder.decode(is, AsnBoolean)
    
    assertEquals(AsnBoolean(true), value)
  }
  
  @Test
  def test_decode_AsnBoolean_02(): Unit = {
    val data = Array[Byte](1, 1, 1)
    val is = new DecodingInputStream(new ByteArrayInputStream(data))
    val value = BerDecoder.decode(is, AsnBoolean)
    
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
    val value = BerDecoder.decode(is, AsnNull)
    assertEquals(AsnNull, value)
  }
  
  @Test
  def test_decode_AsnInteger_00(): Unit = {
    val data = Array[Byte](2, 2, bb(10010110), bb(1000110))
    val is = new DecodingInputStream(new ByteArrayInputStream(data))
    val value = BerDecoder.decode(is, AsnInteger)
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
    assertThrows(classOf[IndexOutOfBoundsException]) {
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
  
  def tag(tc: Int, c: Int, t: Int): Byte = {
    assert(tc < 4)
    assert(c < 2)
    assert(t < 32)
    val result = ((tc << 6) | (c << 5) | t).toByte
    println("tag: " + result)
    return result
  }
  
  @Test
  def test_mine_01(): Unit = {
    import test.asn1.genruntime._
    
    val data = Array[Byte](
        tag(0, 1, 16), 26, // Sequence
          tag(2, 0, 0), 2, 0x9646.toByte,     // Field#0#Integer: -27066
          tag(2, 0, 1), 0,             // Field#1#Real: 0
          tag(2, 0, 2), 3, 65, 66, 67, // Field#2#String: ABC
          tag(2, 0, 3), 4,             // Field#3#Choice:
            tag(2, 0, 1), 2, 0x9646.toByte)   //   Field#1#Integer: -27066
  }
  
  @Test
  def test_mine_02(): Unit = {
    import test.asn1.genruntime._
    
    val data = Array[Byte](0x96.toByte, 0x46.toByte)     // Integer: -27066
    val is = new DecodingInputStream(new ByteArrayInputStream(data))
    var recordedValue = -1L
    val decoder =
      ( OnAsnInteger
        .value { _ => { value: Long => recordedValue = value } }
      )
    decoder.decode(is, data.length)
    assertEquals(-27066, recordedValue)
  }
  
  @Test
  def test_mine_03(): Unit = {
    import test.asn1.genruntime._
    
    val data = Array[Byte](
        tag(2, 0, 0), 2, 0x96.toByte, 0x46.toByte)     // Field#0#Integer: -27066
    val is = new DecodingInputStream(new ByteArrayInputStream(data))
    var recordedValue = -1L
    val decoder =
      ( OnMySequence
        .field0 { _.value { _: (Long => Unit) => { value: Long =>
              recordedValue = value
            } }
         }
      )
    
    decoder.decode(is, data.length)
    
    assertEquals(-27066, recordedValue)
    /*
    val x =
      ( OnMySequence
          .field0 { _ =>
            { field0: OnAsnInteger =>
              ( field0
                  .value { value: Int =>
                    println("field0: " + value)
                  }
              )
            }
          }
          .field1 { field1 =>
            { field1: OnAsnReal =>
              ( field1
                  .value { value: Double =>
                    println("field1: " + value)
                  }
              )
            }
          }
          .field2 { field2 =>
            { field2: OnAsnPrintableString =>
              ( field2
                  .value { value: String =>
                    println("field2: " + value)
                  }
              )
            }
          }
          .field3 { field3 =>
            { field3: OnMyChoice =>
              ( field3
                  .field0 { field0 =>
                    { field0: OnAsnInteger =>
                      ( field0
                          .value { value: Int =>
                            println("field0: " + value)
                          }
                      )
                    }
                  }
                  .field1 { field1 =>
                    { field1: OnAsnReal =>
                      ( field1
                          .value { value: Double =>
                            println("field1: " + value)
                          }
                      )
                    }
                  }
              )
            }
            )*/
  }
  
  @Test
  def test_me_01(): Unit = {
    def foo(f: PartialFunction[Int, Boolean]) = {
      println(f.lift(1))
    }
    
    foo {
      case 2 => true
    }
  }
}
