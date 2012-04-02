package org.asn1gen.runtime.java;

import java.io.IOException;

public class TlvPullParser {
  private final BoundedInputStream is;
  
  public TlvPullParser(final BoundedInputStream is) {
    this.is = is;
  }
  
  public long readTagNo(final int firstTagByte) throws IOException {
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
  
  public long readTagLength() throws IOException {
    final int firstLengthByte = is.read();
    
    if ((firstLengthByte & 0x80) == 0) {
      return firstLengthByte & 0x7f;
    } else {
      final int lengthLength = firstLengthByte & 0x7f;
      long accumulatingLength = 0;
      
      for (int i = 0; i < lengthLength; ++i) {
        accumulatingLength = (accumulatingLength << 8) | (is.read() & 0xff);
      }
      
      return accumulatingLength;
    }
  }
  
  public TlvFrame next() throws IOException {
    if (is.isEmpty()) {
      return null;
    }
    
    final int firstTagByte = is.read();
    
    final TagClass tagClass = TagClass.fromTagByte(firstTagByte);
    final TagForm tagForm = TagForm.fromTagByte(firstTagByte);
    final long tagNo = readTagNo(firstTagByte);
    final long tagLength = readTagLength();
    
    return new TlvFrame(tagClass, tagForm, tagNo, tagLength);
  }
}
