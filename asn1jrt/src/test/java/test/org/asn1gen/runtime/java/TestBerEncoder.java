package test.org.asn1gen.runtime.java;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.asn1gen.runtime.java.AsnClass;
import org.asn1gen.runtime.java.AsnForm;
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
  public void test_18_2_1_boolean_1() {
    
  }
}
