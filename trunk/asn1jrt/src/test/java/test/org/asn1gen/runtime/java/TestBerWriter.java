package test.org.asn1gen.runtime.java;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import junit.framework.Assert;

import org.asn1gen.runtime.java.BerWriter;
import org.junit.Test;

import scala.actors.threadpool.Arrays;

public class TestBerWriter {
  @Test
  public void writeByte_00() throws IOException {
    final BerWriter berWriter = BerWriter.writeByte((byte)0);
    Assert.assertEquals("BerWriter has correct length", 1, berWriter.length);
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    berWriter.write(os);
    os.flush();
    Assert.assertTrue(Arrays.equals(os.toByteArray(), new byte[] { 0 }));
  }
}
