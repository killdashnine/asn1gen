package test.org.asn1gen.runtime.java;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import junit.framework.Assert;

import org.asn1gen.runtime.java.BerWriter;
import org.junit.Test;

import scala.actors.threadpool.Arrays;

public class TestBerWriter {
  @Test
  public void bbyte_00() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.bbyte((byte)0);
    Assert.assertEquals("BerWriter has correct length", 1, berWriter.length);
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(os);
    berWriter.write(dos);
    dos.flush();
    os.flush();
    Assert.assertTrue(Arrays.equals(os.toByteArray(), new byte[] { 0 }));
  }

  @Test
  public void bbyte_01() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.bbyte((byte)1);
    Assert.assertEquals("BerWriter has correct length", 1, berWriter.length);
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(os);
    berWriter.write(dos);
    dos.flush();
    os.flush();
    Assert.assertTrue(Arrays.equals(os.toByteArray(), new byte[] { 1 }));
  }

  @Test
  public void bbyte_02() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.bbyte((byte)2);
    Assert.assertEquals("BerWriter has correct length", 1, berWriter.length);
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(os);
    berWriter.write(dos);
    dos.flush();
    os.flush();
    Assert.assertTrue(Arrays.equals(os.toByteArray(), new byte[] { 2 }));
  }

  @Test
  public void bbyte_03() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.bbyte((byte)0x7f);
    Assert.assertEquals("BerWriter has correct length", 1, berWriter.length);
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(os);
    berWriter.write(dos);
    dos.flush();
    os.flush();
    Assert.assertTrue(Arrays.equals(os.toByteArray(), new byte[] { 0x7f }));
  }

  @Test
  public void bbyte_04() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.bbyte((byte)0xff);
    Assert.assertEquals("BerWriter has correct length", 1, berWriter.length);
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(os);
    berWriter.write(dos);
    dos.flush();
    os.flush();
    Assert.assertTrue(Arrays.equals(os.toByteArray(), new byte[] { (byte)0xff }));
  }

  @Test
  public void sbyte_00() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.sbyte((short)0);
    Assert.assertEquals("BerWriter has correct length", 1, berWriter.length);
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(os);
    berWriter.write(dos);
    dos.flush();
    os.flush();
    Assert.assertTrue(Arrays.equals(os.toByteArray(), new byte[] { 0 }));
  }

  @Test
  public void sbyte_01() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.sbyte((short)1);
    Assert.assertEquals("BerWriter has correct length", 1, berWriter.length);
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(os);
    berWriter.write(dos);
    dos.flush();
    os.flush();
    Assert.assertTrue(Arrays.equals(os.toByteArray(), new byte[] { 1 }));
  }

  @Test
  public void sbyte_02() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.sbyte((short)2);
    Assert.assertEquals("BerWriter has correct length", 1, berWriter.length);
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(os);
    berWriter.write(dos);
    dos.flush();
    os.flush();
    Assert.assertTrue(Arrays.equals(os.toByteArray(), new byte[] { 2 }));
  }

  @Test
  public void sbyte_03() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.sbyte((short)0x7f);
    Assert.assertEquals("BerWriter has correct length", 1, berWriter.length);
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(os);
    berWriter.write(dos);
    dos.flush();
    os.flush();
    Assert.assertTrue(Arrays.equals(os.toByteArray(), new byte[] { 0x7f }));
  }

  @Test
  public void sbyte_04() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.sbyte((short)0xff);
    Assert.assertEquals("BerWriter has correct length", 1, berWriter.length);
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(os);
    berWriter.write(dos);
    dos.flush();
    os.flush();
    Assert.assertTrue(Arrays.equals(os.toByteArray(), new byte[] { (byte)0xff }));
  }

  @Test
  public void bbytes_00() throws IOException {
    final BerWriter berWriter = BerWriter.EMPTY.bbytes((byte)0, (byte)1, (byte)0x7f, (byte)0xff);
    Assert.assertEquals("BerWriter has correct length", 4, berWriter.length);
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(os);
    berWriter.write(dos);
    dos.flush();
    os.flush();
    Assert.assertTrue(Arrays.equals(os.toByteArray(), new byte[] { 0, 1, 0x7f, (byte)0xff }));
  }

}
