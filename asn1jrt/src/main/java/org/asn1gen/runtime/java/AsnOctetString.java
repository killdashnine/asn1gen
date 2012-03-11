package org.asn1gen.runtime.java;

import java.nio.charset.Charset;

public class AsnOctetString implements AsnType {
  public static final AsnOctetString EMPTY = new AsnOctetString(new byte[0]);
  
  public final byte[] value;
  
  public AsnOctetString(final byte[] value) {
    this.value = value;
  }
  
  public String value(final Charset charset) {
    return new String(this.value, charset);
  }
}
