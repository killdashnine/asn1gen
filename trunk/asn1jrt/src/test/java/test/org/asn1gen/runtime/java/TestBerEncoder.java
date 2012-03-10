package test.org.asn1gen.runtime.java;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.asn1gen.runtime.java.AsnBoolean;
import org.asn1gen.runtime.java.AsnClass;
import org.asn1gen.runtime.java.AsnForm;
import org.asn1gen.runtime.java.AsnNull;
import org.asn1gen.runtime.java.BerEncoder;
import org.asn1gen.runtime.java.BerWriter;
import org.junit.Assert;
import org.junit.Test;

public class TestBerEncoder {
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
    final BerWriter berWriter = BerEncoder.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {0}, result);
  }
  
  @Test
  public void test_18_1_tag_b() throws IOException {
    final BerWriter berWriter = BerEncoder.tag(AsnClass.APPLICATION, AsnForm.PRIMITIVE, 0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {0x40}, result);
  }
  
  @Test
  public void test_18_1_tag_c() throws IOException {
    final BerWriter berWriter = BerEncoder.tag(AsnClass.CONTEXT_SPECIFIC, AsnForm.PRIMITIVE, 0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {(byte)0x80}, result);
  }
  
  @Test
  public void test_18_1_tag_d() throws IOException {
    final BerWriter berWriter = BerEncoder.tag(AsnClass.PRIVATE, AsnForm.PRIMITIVE, 0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {(byte)0xc0}, result);
  }
  
  @Test
  public void test_18_1_tag_e() throws IOException {
    final BerWriter berWriter = BerEncoder.tag(AsnClass.UNIVERSAL, AsnForm.CONSTRUCTED, 0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {0x20}, result);
  }
  
  @Test
  public void test_18_1_tag_f() throws IOException {
    final BerWriter berWriter = BerEncoder.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 1);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {0x01}, result);
  }
  
  @Test
  public void test_18_1_tag_g() throws IOException {
    final BerWriter berWriter = BerEncoder.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 30);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {30}, result);
  }
  
  @Test
  public void test_18_1_tag_h() throws IOException {
    final BerWriter berWriter = BerEncoder.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 31);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {31, 31}, result);
  }
  
  @Test
  public void test_18_1_tag_i() throws IOException {
    final BerWriter berWriter = BerEncoder.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 0x7f);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {31, 0x7f}, result);
  }
  
  @Test
  public void test_18_1_tag_j() throws IOException {
    final BerWriter berWriter = BerEncoder.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 0x80);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {31, (byte)0x81, 0x00}, result);
  }
  
  @Test
  public void test_18_1_tag_k() throws IOException {
    final BerWriter berWriter = BerEncoder.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, 0x4080);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {31, (byte)0x81, (byte)0x81, 0x00}, result);
  }

  @Test
  public void test_18_1_tag_l() throws IOException {
    final BerWriter berWriter = BerEncoder.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, Long.MAX_VALUE);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {31, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, 0x7f}, result);
  }
  
  @Test(expected=IllegalArgumentException.class)
  public void test_18_1_tag_m() throws IOException {
    final BerWriter berWriter = BerEncoder.tag(AsnClass.UNIVERSAL, AsnForm.PRIMITIVE, -1);
    writeToByteArray(berWriter);
  }

  @Test
  public void test_18_1_length_a() throws IOException {
    final BerWriter berWriter = BerEncoder.length(0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {0}, result);
  }

  @Test
  public void test_18_1_length_b() throws IOException {
    final BerWriter berWriter = BerEncoder.length(0x7f);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {0x7f}, result);
  }

  @Test
  public void test_18_1_length_c() throws IOException {
    final BerWriter berWriter = BerEncoder.length(0x80);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {(byte)0x81, (byte)0x80}, result);
  }

  @Test
  public void test_18_1_length_d() throws IOException {
    final BerWriter berWriter = BerEncoder.length(0x81);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {(byte)0x81, (byte)0x81}, result);
  }

  @Test
  public void test_18_1_length_e() throws IOException {
    final BerWriter berWriter = BerEncoder.length(0xff);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {(byte)0x81, (byte)0xff}, result);
  }
  
  @Test
  public void test_18_1_length_f() throws IOException {
    final BerWriter berWriter = BerEncoder.length(0x100);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {(byte)0x82, (byte)0x01, (byte)0x00}, result);
  }
  
  @Test
  public void test_18_1_length_g() throws IOException {
    final BerWriter berWriter = BerEncoder.length(0xffff);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {(byte)0x82, (byte)0xff, (byte)0xff}, result);
  }
  
  @Test
  public void test_18_1_length_h() throws IOException {
    final BerWriter berWriter = BerEncoder.length(0x76543210);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {(byte)0x84, (byte)0x76, (byte)0x54, (byte)0x32, (byte)0x10}, result);
  }
  
  @Test(expected=IllegalArgumentException.class)
  public void test_18_1_length_i() throws IOException {
    final BerWriter berWriter = BerEncoder.length(-1);
    writeToByteArray(berWriter);
  }
  
  @Test
  public void test_18_2_1_boolean_a() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(false);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {1, 1, 0}, result);
  }
  
  @Test
  public void test_18_2_1_boolean_b() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(true);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertEquals(3, result.length);
    Assert.assertEquals(1, result[0]);
    Assert.assertEquals(1, result[1]);
    Assert.assertTrue(0 != result[2]);
  }
  
  @Test
  public void test_18_2_1_boolean_c() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(AsnBoolean.FALSE);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {1, 1, 0}, result);
  }
  
  @Test
  public void test_18_2_1_boolean_d() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(AsnBoolean.TRUE);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertEquals(3, result.length);
    Assert.assertEquals(1, result[0]);
    Assert.assertEquals(1, result[1]);
    Assert.assertTrue(0 != result[2]);
  }
  
  @Test
  public void test_18_2_2_null() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(AsnNull.EMPTY);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {5, 0}, result);
  }

  @Test
  public void test_18_2_3_integer_a() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(-27066);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {2, 2, (byte)0x96, 0x46}, result);
  }

  @Test
  public void test_18_2_3_integer_b() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(5256);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {2, 2, 0x14, (byte)0x88}, result);
  }

  @Test
  public void test_18_2_3_integer_c() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {2, 1, 0}, result);
  }

  @Test
  public void test_18_2_3_integer_d() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(0xffffffffffabafbfL);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {2, 3, (byte)0xab, (byte)0xaf, (byte)0xbf}, result);
  }
  
  @Test
  public void test_18_2_3_integer_e() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(0x545040);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {2, 3, (byte)0x54, (byte)0x50, (byte)0x40}, result);
  }
  
  @Test
  public void test_18_2_5_real_a() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(0.0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {9, 0}, result);
  }
  
  @Test
  public void test_18_2_5_real_b() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(Double.POSITIVE_INFINITY);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {9, 1, 0x40}, result);
  }
  
  @Test
  public void test_18_2_5_real_c() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(Double.NEGATIVE_INFINITY);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {9, 1, 0x41}, result);
  }
  
  @Test
  public void test_18_2_5_real_d() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(1.0);
    final byte[] result = writeToByteArray(berWriter);
    Assert.assertArrayEquals(new byte[] {9, 3, (byte)0x80, 0, 1}, result);
  }
  
  @Test
  public void test_18_2_5_real_e() throws IOException {
    final BerWriter berWriter = BerEncoder.encode(2.0);
    final byte[] result = writeToByteArray(berWriter);
    System.out.print("result: "); berWriter.dumpln();
    Assert.assertArrayEquals(new byte[] {9, 3, (byte)0x80, 1, 1}, result);
  }
  
  public void printRawDouble(final double value) {
    long l1 = Double.doubleToLongBits(value);
    int i3 = (int)((Double.doubleToLongBits(value) >> 52) & 0x7FF) - 1075;
    long l2 = i3 == 0 ? (l1 & 0xFFFFFFFF) << 1 : l1 & 0xFFFFFFFF | 0x0;
    System.out.println(value + " " + Double.doubleToLongBits(value) + " " + i3 + " " + l2);
  }
  
  @Test
  public void listDoubles() throws IOException {
    //printRawDouble(Double.POSITIVE_INFINITY);
    //printRawDouble(Double.NEGATIVE_INFINITY);
    //printRawDouble(0.0);
    //printRawDouble(1.0);
    //printRawDouble(2.0);
    //printRawDouble(3.0);
    //printRawDouble(4.0);
    //printRawDouble(5.0);
    //printRawDouble(6.0);
    //printRawDouble(7.0);
  }
}
