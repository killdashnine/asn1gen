package moo.codec.ASNEXAMPLES;

import org.asn1gen.runtime.java.AsnException;
import org.asn1gen.runtime.java.ByteArrayWindow;
import org.asn1gen.runtime.java.TLV;

import moo.model.ASNEXAMPLES.BookCover;

public class BerToAsn {
  public static BookCover decode(
      final BookCover value,
      final ByteArrayWindow window) throws AsnException {
    return decodePart(value, window);
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
