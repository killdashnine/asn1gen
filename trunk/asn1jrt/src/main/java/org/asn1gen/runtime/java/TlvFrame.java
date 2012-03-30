package org.asn1gen.runtime.java;

public class TlvFrame {
  public final TlvFrame parent;
  public final TagClass tagClass;
  public final TagForm tagForm;
  public final long tagNo;
  public final long length;
  
  public TlvFrame(final TlvFrame parent, final TagClass tagClass, final TagForm tagForm, final long tagNo, final long length) {
    this.parent = parent;
    this.tagClass = tagClass;
    this.tagForm = tagForm;
    this.tagNo = tagNo;
    this.length = length;
  }

  public TlvFrame(final TagClass tagClass, final TagForm tagForm, final long tagNo, final long length) {
    this(null, tagClass, tagForm, tagNo, length);
  }
}
