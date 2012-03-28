/* This file was generated by asn1gen */

package moo.value;

import org.asn1gen.runtime.java.*;

import static org.asn1gen.runtime.java.Statics.*;

import moo.model.ASNEXAMPLES.*;
import static moo.model.ASNEXAMPLES.BookCover.*;

@SuppressWarnings("unused")
public class ASNEXAMPLES {
  public static final Book defaultBook = Book.EMPTY
    .withIsbn(new AsnOctetString("default isbn"))
    .withTitle(new AsnOctetString("default title"))
    .withAuthor(new AsnOctetString("default author"))
    .withCover(paperBack)
    .withIsInPrint(AsnBoolean.TRUE);
  public static final AsnOctetString defaultOctetString = new AsnOctetString("Hello world");
  public static final AsnBoolean defaultBooleanFalse = AsnBoolean.FALSE;
  public static final AsnBoolean defaultBooleanTrue = AsnBoolean.TRUE;
  public static final AsnInteger defaultInteger = new AsnInteger(123);
}
