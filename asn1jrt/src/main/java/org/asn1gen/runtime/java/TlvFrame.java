package org.asn1gen.runtime.java;

import java.io.IOException;

public class TlvFrame {
  public final TagClass tagClass;
  public final TagForm tagForm;
  public final long tagNo;
  public final long length;
  public final ByteArrayWindow data;
  public final ByteArrayWindow remainder;
  
  public TlvFrame(
      final TagClass tagClass,
      final TagForm tagForm,
      final long tagNo,
      final long length,
      final ByteArrayWindow data,
      final ByteArrayWindow remainder) {
    this.tagClass = tagClass;
    this.tagForm = tagForm;
    this.tagNo = tagNo;
    this.length = length;
    this.data = data;
    this.remainder = remainder;
  }
  
  public static TlvFrame readFrom(final ByteArrayWindow window) throws IOException {
    final ByteArrayWindowInputStream is = window.getInputStream();
    
    if (is.isEmpty()) {
      return null;
    }
    
    final int firstTagByte = is.read();
    
    final TagClass tagClass = TagClass.fromTagByte(firstTagByte);
    final TagForm tagForm = TagForm.fromTagByte(firstTagByte);
    final long tagNo = readTagNo(firstTagByte, is);
    final int tagLength = readTagLength(is);
    
    final ByteArrayWindow nextWindow = is.getWindow();
    
    return new TlvFrame(
        tagClass, tagForm, tagNo, tagLength,
        nextWindow.until(tagLength), nextWindow.from(tagLength));
  }
  
  public TlvFrame firstChild() throws IOException {
    return readFrom(data);
  }
  
  public TlvFrame next() throws IOException {
    return readFrom(remainder);
  }
  
  public static long readTagNo(
      final int firstTagByte,
      final ByteArrayWindowInputStream is) throws IOException {
    final long tagNoPart = firstTagByte & 0x1f;
    
    if (tagNoPart < 32) {
      return tagNoPart;
    } else {
      int accumulatingTagNo = 0;
      
      while (true) {
        final int nextTagByte = is.read();
        
        accumulatingTagNo = (accumulatingTagNo << 7) | (nextTagByte & 0x7f);
        
        if ((nextTagByte & 0x80) == 0) {
          break;
        }
      }
      
      return accumulatingTagNo;
    }
  }
  
  public static int readTagLength(final ByteArrayWindowInputStream is) throws IOException {
    final int firstLengthByte = is.read();
    
    if ((firstLengthByte & 0x80) == 0) {
      return firstLengthByte & 0x7f;
    } else {
      final int lengthLength = firstLengthByte & 0x7f;
      int accumulatingLength = 0;
      
      for (int i = 0; i < lengthLength; ++i) {
        accumulatingLength = (accumulatingLength << 8) | (is.read() & 0xff);
      }
      
      return accumulatingLength;
    }
  }
}
