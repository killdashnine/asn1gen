package test.moo.codec.asnexamples;

import moo.model.ASNEXAMPLES.Book;
import static moo.codec.ASNEXAMPLES.AsnToBerEncoder.*;

import org.asn1gen.runtime.java.AsnException;
import org.asn1gen.runtime.java.BerWriter;
import org.junit.Test;

public class TestAsnToBerEncoder {
  @Test
  public void encodeBook() throws AsnException {
    final BerWriter dataWriter = encode(Book.EMPTY);
    dataWriter.dump();
  }
}
