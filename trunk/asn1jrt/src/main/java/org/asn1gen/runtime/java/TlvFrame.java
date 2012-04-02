package org.asn1gen.runtime.java;

public class TlvFrame {
  public final TagClass tagClass;
  public final TagForm tagForm;
  public final long tagNo;
  public final long length;
  
  public TlvFrame(final TagClass tagClass, final TagForm tagForm, final long tagNo, final long length) {
    this.tagClass = tagClass;
    this.tagForm = tagForm;
    this.tagNo = tagNo;
    this.length = length;
  }
}
