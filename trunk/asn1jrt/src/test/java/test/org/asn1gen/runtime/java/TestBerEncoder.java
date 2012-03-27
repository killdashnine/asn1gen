package test.org.asn1gen.runtime.java;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.asn1gen.runtime.java.AsnBitString;
import org.asn1gen.runtime.java.AsnBoolean;
import org.asn1gen.runtime.java.AsnClass;
import org.asn1gen.runtime.java.AsnForm;
import org.asn1gen.runtime.java.AsnNull;
import org.asn1gen.runtime.java.AsnToBerEncoder;
import org.asn1gen.runtime.java.AsnUtf8String;
import org.asn1gen.runtime.java.BerWriter;
import org.junit.Assert;
import org.junit.Test;

public class TestBerEncoder {
  public static byte[] ibytes(final int ...values) {
    final byte[] bytes = new byte[values.length];
    
    for (int i = 0; i < values.length; ++i) {
      bytes[i] = (byte)values[i];
    }
    
    return bytes;
  }
  
  public static byte[] writeToByteArray(final BerWriter berWriter) throws IOException {
    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(baos);
    berWriter.write(dos);
    dos.flush();
    baos.flush();
    return baos.toByteArray();
  }
  
  @Test
  public void test_18_1_tag_a() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x00), result);
  }
  
  @Test
  public void test_18_1_tag_b() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.APPLICATION, AsnForm.PRIMITIVE, 0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x40), result);
  }
  
  @Test
  public void test_18_1_tag_c() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.CONTEXT_SPECIFIC, AsnForm.PRIMITIVE, 0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x80), result);
  }
  
  @Test
  public void test_18_1_tag_d() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.PRIVATE, AsnForm.PRIMITIVE, 0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0xc0), result);
  }
  
  @Test
  public void test_18_1_tag_e() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.UNIVERSAL, AsnForm.CONSTRUCTED, 0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x20), result);
  }
  
  @Test
  public void test_18_1_tag_f() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 1);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x01), result);
  }
  
  @Test
  public void test_18_1_tag_g() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 30);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(30), result);
  }
  
  @Test
  public void test_18_1_tag_h() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 31);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(31, 31), result);
  }
  
  @Test
  public void test_18_1_tag_i() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 0x7f);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(31, 0x7f), result);
  }
  
  @Test
  public void test_18_1_tag_j() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 0x80);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(31, 0x81, 0x00), result);
  }
  
  @Test
  public void test_18_1_tag_k() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 0x4080);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(31, 0x81, 0x81, 0x00), result);
  }

  @Test
  public void test_18_1_tag_l() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, Long.MAX_VALUE);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(31, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f), result);
  }
  
  @Test(expected=IllegalArgumentException.class)
  public void test_18_1_tag_m() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, -1);
    writeToByteArray(berWriter);
  }

  @Test
  public void test_18_1_length_a() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.length(0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0), result);
  }

  @Test
  public void test_18_1_length_b() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.length(0x7f);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x7f), result);
  }

  @Test
  public void test_18_1_length_c() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.length(0x80);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x81, 0x00), result);
  }

  @Test
  public void test_18_1_length_d() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.length(0x81);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x81, 0x01), result);
  }

  @Test
  public void test_18_1_length_e() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.length(0xff);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x81, 0x7f), result);
  }
  
  @Test
  public void test_18_1_length_f() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.length(0x100);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x82, 0x00), result);
  }
  
  @Test
  public void test_18_1_length_g() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.length(0xffff);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x83, 0xff, 0x7f), result);
  }
  
  @Test
  public void test_18_1_length_h() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.length(0x76543210);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x87, 0xb2, 0xd0, 0xe4, 0x10), result);
  }
  
  @Test(expected=IllegalArgumentException.class)
  public void test_18_1_length_i() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.length(-1);
    writeToByteArray(berWriter);
  }
  
  @Test
  public void test_18_2_1_boolean_a() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(false);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(1, 1, 0), result);
  }
  
  @Test
  public void test_18_2_1_boolean_b() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(true);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertEquals(3, result.length);
    Assert.assertEquals(1, result[0]);
    Assert.assertEquals(1, result[1]);
    Assert.assertTrue(0 != result[2]);
  }
  
  @Test
  public void test_18_2_1_boolean_c() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(AsnBoolean.FALSE);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(1, 1, 0), result);
  }
  
  @Test
  public void test_18_2_1_boolean_d() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(AsnBoolean.TRUE);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertEquals(3, result.length);
    Assert.assertEquals(1, result[0]);
    Assert.assertEquals(1, result[1]);
    Assert.assertTrue(0 != result[2]);
  }
  
  @Test
  public void test_18_2_2_null() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(AsnNull.EMPTY);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(5, 0), result);
  }

  @Test
  public void test_18_2_3_integer_a() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(-27066);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(2, 2, 0x96, 0x46), result);
  }

  @Test
  public void test_18_2_3_integer_b() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(5256);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(2, 2, 0x14, 0x88), result);
  }

  @Test
  public void test_18_2_3_integer_c() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(2, 1, 0), result);
  }

  @Test
  public void test_18_2_3_integer_d() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(0xffffffffffabafbfL);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x02, 0x03, 0xab, 0xaf, 0xbf), result);
  }
  
  @Test
  public void test_18_2_3_integer_e() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(0x545040);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x02, 0x03, 0x54, 0x50, 0x40), result);
  }
  
  @Test
  public void test_18_2_5_real_a() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(0.0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x00), result);
  }
  
  @Test
  public void test_18_2_5_real_b() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(Double.POSITIVE_INFINITY);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x01, 0x40), result);
  }
  
  @Test
  public void test_18_2_5_real_c() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(Double.NEGATIVE_INFINITY);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x01, 0x41), result);
  }
  
  @Test
  public void test_18_2_5_real_d() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(1.0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x03, 0x80, 0x00, 0x01), result);
  }

  @Test
  public void test_18_2_5_real_e() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(2.0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x03, 0x80, 0x01, 0x01), result);
  }

  @Test
  public void test_18_2_5_real_f() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(3.0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x03, 0x80, 0x00, 0x03), result);
  }
  
  @Test
  public void test_18_2_5_real_g() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(0.1);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x09, 0x80, 0xc9, 0x0c, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcd), result);
  }

  @Test
  public void test_18_2_5_real_h() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(0.2);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x09, 0x80, 0xca, 0x0c, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcd), result);
  }

  @Test
  public void test_18_2_5_real_i() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(0.3);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x09, 0x80, 0xca, 0x13, 0x33, 0x33, 0x33, 0x33, 0x33, 0x33), result);
  }

  @Test
  public void test_18_2_5_real_j() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(1.1);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x09, 0x80, 0xcd, 0x08, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcd), result);
  }

  @Test
  public void test_18_2_5_real_k() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(-1.0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x03, 0xc0, 0x00, 0x01), result);
  }

  @Test
  public void test_18_2_5_real_l() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(-3.0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x03, 0xc0, 0x00, 0x03), result);
  }

  @Test
  public void test_18_2_5_real_m() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(-4.0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x03, 0xc0, 0x02, 0x01), result);
  }

  @Test
  public void test_18_2_5_real_n() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(-0.1);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x09, 0xc0, 0xc9, 0x0c, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcd), result);
  }

  @Test
  public void test_18_2_5_real_o() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(-0.2);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x09, 0xc0, 0xca, 0x0c, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcd), result);
  }

  @Test
  public void test_18_2_5_real_p() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(-0.3);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x09, 0x09, 0xc0, 0xca, 0x13, 0x33, 0x33, 0x33, 0x33, 0x33, 0x33), result);
  }
  
  @Test
  public void test_18_2_6_bitstring_a() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(new AsnBitString(0xff, 8));
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x03, 0x02, 0x00, 0xff), result);
  }

  @Test
  public void test_18_2_6_bitstring_b() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(new AsnBitString(0xff, 11));
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x03, 0x03, 0x05, 0x1f, 0xe0), result);
  }

  @Test
  public void test_18_2_6_bitstring_c() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(new AsnBitString(0xdeadbeef, 32));
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x03, 0x05, 0x00, 0xde, 0xad, 0xbe, 0xef), result);
  }

  @Test
  public void test_18_2_6_bitstring_d() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(new AsnBitString(0xdeadbeefdeadbeefL, 64));
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x03, 0x09, 0x00, 0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef), result);
  }

  @Test
  public void test_18_2_6_bitstring_e() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(new AsnBitString(0x5eadbeefdeadbeefL, 63));
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x03, 0x09, 0x01, 0xbd, 0x5b, 0x7d, 0xdf, 0xbd, 0x5b, 0x7d, 0xde), result);
  }

  @Test
  public void test_18_2_7_octetstring_a() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(new AsnUtf8String(""));
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x0c, 0x00), result);
  }

  @Test
  public void test_18_2_7_octetstring_b() throws IOException {
    final BerWriter berWriter = AsnToBerEncoder.encode(new AsnUtf8String("Hello world"));
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(ibytes(0x0c, 0x0b, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64), result);
  }

  @Test
  public void testTrailingZeros() {
    Assert.assertEquals(2, AsnToBerEncoder.trailingZeros(0x0000000000000000L, 1));
    Assert.assertEquals(0, AsnToBerEncoder.trailingZeros(0x0000000000000001L, 1));
    Assert.assertEquals(1, AsnToBerEncoder.trailingZeros(0x0000000000000002L, 1));
    Assert.assertEquals(0, AsnToBerEncoder.trailingZeros(0x0000000000000003L, 1));
    
    Assert.assertEquals(4, AsnToBerEncoder.trailingZeros(0x0000000000000000L, 2));
    Assert.assertEquals(0, AsnToBerEncoder.trailingZeros(0x0000000000000001L, 2));
    Assert.assertEquals(1, AsnToBerEncoder.trailingZeros(0x0000000000000002L, 2));
    Assert.assertEquals(0, AsnToBerEncoder.trailingZeros(0x0000000000000003L, 2));
    Assert.assertEquals(2, AsnToBerEncoder.trailingZeros(0x0000000000000004L, 2));
    Assert.assertEquals(0, AsnToBerEncoder.trailingZeros(0x0000000000000005L, 2));
    Assert.assertEquals(1, AsnToBerEncoder.trailingZeros(0x0000000000000006L, 2));
    Assert.assertEquals(0, AsnToBerEncoder.trailingZeros(0x0000000000000007L, 2));
    Assert.assertEquals(3, AsnToBerEncoder.trailingZeros(0x0000000000000008L, 2));
    Assert.assertEquals(4, AsnToBerEncoder.trailingZeros(0x0000000000000000L, 2));
    
    Assert.assertEquals(0, AsnToBerEncoder.trailingZeros(0x0000000000000009L, 2));
    Assert.assertEquals(1, AsnToBerEncoder.trailingZeros(0x000000000000000aL, 2));
    Assert.assertEquals(0, AsnToBerEncoder.trailingZeros(0x000000000000000bL, 2));
    Assert.assertEquals(2, AsnToBerEncoder.trailingZeros(0x000000000000000cL, 2));
    Assert.assertEquals(0, AsnToBerEncoder.trailingZeros(0x000000000000000dL, 2));
    Assert.assertEquals(1, AsnToBerEncoder.trailingZeros(0x000000000000000eL, 2));
    Assert.assertEquals(0, AsnToBerEncoder.trailingZeros(0x000000000000000fL, 2));
    Assert.assertEquals(4, AsnToBerEncoder.trailingZeros(0x0000000000000010L, 2));
    
    Assert.assertEquals(64, AsnToBerEncoder.trailingZeros(0x0000000000000000L));
    for (long j = 1; j < 0xfffffL; j += 2) {
      for (int i = 0; i < 64; ++i) {
        Assert.assertEquals(i, AsnToBerEncoder.trailingZeros(j << i));
      }
    }
  }
}
