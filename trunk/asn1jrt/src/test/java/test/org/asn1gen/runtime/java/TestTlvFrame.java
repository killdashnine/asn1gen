package test.org.asn1gen.runtime.java;

import java.io.IOException;

import junit.framework.Assert;

import org.asn1gen.runtime.java.ByteArrayWindow;
import org.asn1gen.runtime.java.TagClass;
import org.asn1gen.runtime.java.TagForm;
import org.asn1gen.runtime.java.TlvFrame;
import org.junit.Test;

public class TestTlvFrame {
  @Test
  public void tlvPullParser() throws IOException {
    System.err.println("foo");
    final byte[] data = new byte[] { 0x31, 0x0c, 0x04, 0x00, 0x04, 0x00, 0x04, 0x00, 0x0a, 0x01, 0x00, 0x01, 0x01, 0x00 };
    final TlvFrame frame1 = TlvFrame.readFrom(ByteArrayWindow.to(data));
    Assert.assertEquals(TagClass.UNIVERSAL, frame1.tagClass);
    Assert.assertEquals(TagForm.CONSTRUCTED, frame1.tagForm);
    Assert.assertEquals(17, frame1.tagNo);
    Assert.assertEquals(12, frame1.length);
    final TlvFrame childFrame1 = frame1.firstChild();
    Assert.assertEquals(TagClass.UNIVERSAL, childFrame1.tagClass);
    Assert.assertEquals(TagForm.PRIMITIVE, childFrame1.tagForm);
    Assert.assertEquals(4, childFrame1.tagNo);
    Assert.assertEquals(0, childFrame1.length);
    final TlvFrame childFrame2 = childFrame1.next();
    Assert.assertEquals(TagClass.UNIVERSAL, childFrame2.tagClass);
    Assert.assertEquals(TagForm.PRIMITIVE, childFrame2.tagForm);
    Assert.assertEquals(4, childFrame2.tagNo);
    Assert.assertEquals(0, childFrame2.length);
    final TlvFrame childFrame3 = childFrame2.next();
    Assert.assertEquals(TagClass.UNIVERSAL, childFrame3.tagClass);
    Assert.assertEquals(TagForm.PRIMITIVE, childFrame3.tagForm);
    Assert.assertEquals(4, childFrame3.tagNo);
    Assert.assertEquals(0, childFrame3.length);
    final TlvFrame childFrame4 = childFrame3.next();
    Assert.assertEquals(TagClass.UNIVERSAL, childFrame4.tagClass);
    Assert.assertEquals(TagForm.PRIMITIVE, childFrame4.tagForm);
    Assert.assertEquals(10, childFrame4.tagNo);
    Assert.assertEquals(1, childFrame4.length);
    final TlvFrame frame2 = frame1.next();
    Assert.assertEquals(null, frame2);
  }
}
