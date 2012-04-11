package moo.codec.ASNEXAMPLES;

import org.asn1gen.runtime.java.AsnException;
import org.asn1gen.runtime.java.ByteArrayWindow;
import org.asn1gen.runtime.java.DetailedTlvFrame;
import org.asn1gen.runtime.java.TLV;
import org.asn1gen.runtime.java.TagClass;
import org.asn1gen.runtime.java.TagForm;
import org.asn1gen.runtime.java.TlvFrame;

import moo.model.ASNEXAMPLES.BookCover;

public class BerToAsn {
  public static BookCover decode(
      final BookCover value,
      final ByteArrayWindow window) throws AsnException {
    final DetailedTlvFrame detailedFrame = TLV.readTlv(window);
    final TlvFrame frame = detailedFrame.frame;
    
    if (frame.tagClass != TagClass.UNIVERSAL) {
      throw new AsnException();
    }
    
    if (frame.tagForm != TagForm.PRIMITIVE) {
      throw new AsnException();
    }
    
    if (frame.tagNo != 10) {
      throw new AsnException();
    }
    
    return decodePart(value, frame.value);
  }
  
  public static BookCover decodePart(
      final BookCover value,
      final ByteArrayWindow window) throws AsnException {
    final long intValue = TLV.longValue(window);
    
    if (intValue == BookCover.hardCover.value) {
      return BookCover.hardCover;
    } else if (intValue == BookCover.paperBack.value) {
      return BookCover.paperBack;
    } else {
      throw new AsnException();
    }
  }
}
