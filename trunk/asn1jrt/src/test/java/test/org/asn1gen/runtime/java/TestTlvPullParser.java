package test.org.asn1gen.runtime.java;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import junit.framework.Assert;

import org.asn1gen.runtime.java.IndexedInputStream;
import org.asn1gen.runtime.java.TagClass;
import org.asn1gen.runtime.java.TagForm;
import org.asn1gen.runtime.java.TlvFrame;
import org.asn1gen.runtime.java.TlvPullParser;
import org.junit.Test;

public class TestTlvPullParser {
  @Test
  public void testTlvPullParser() throws IOException {
    final byte[] data = new byte[] { 0x31, 0x0c, 0x04, 0x00, 0x04, 0x00, 0x04, 0x00, 0x0a, 0x01, 0x00, 0x01, 0x01, 0x00 };
    final ByteArrayInputStream bais = new ByteArrayInputStream(data);
    final IndexedInputStream iis = new IndexedInputStream(bais);
    final TlvPullParser parser = new TlvPullParser(iis.unbounded());
    final TlvFrame frame = parser.next();
    Assert.assertEquals(TagClass.UNIVERSAL, frame.tagClass);
    Assert.assertEquals(TagForm.CONSTRUCTED, frame.tagForm);
    Assert.assertEquals(12, frame.length);
  }
}
