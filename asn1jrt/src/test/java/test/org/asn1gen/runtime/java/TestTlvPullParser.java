package test.org.asn1gen.runtime.java;

import java.io.IOException;

import junit.framework.Assert;

import org.asn1gen.runtime.java.ByteArrayWindow;
import org.asn1gen.runtime.java.TagClass;
import org.asn1gen.runtime.java.TagForm;
import org.asn1gen.runtime.java.TlvFrame;
import org.junit.Test;

public class TestTlvPullParser {
  @Test
  public void tlvPullParser() throws IOException {
    System.err.println("foo");
    final byte[] data = new byte[] { 0x31, 0x0c, 0x04, 0x00, 0x04, 0x00, 0x04, 0x00, 0x0a, 0x01, 0x00, 0x01, 0x01, 0x00 };
    final TlvFrame frame = TlvFrame.readFrom(ByteArrayWindow.to(data));
    Assert.assertEquals(TagClass.UNIVERSAL, frame.tagClass);
    Assert.assertEquals(TagForm.CONSTRUCTED, frame.tagForm);
    Assert.assertEquals(17, frame.tagNo);
    Assert.assertEquals(12, frame.length);
    final TlvFrame frame2 = frame.next();
    Assert.assertEquals(null, frame2);
  }
}
