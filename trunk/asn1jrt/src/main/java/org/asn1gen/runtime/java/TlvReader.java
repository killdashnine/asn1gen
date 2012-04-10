package org.asn1gen.runtime.java;

public class TlvReader {
  public static <T> ByteArrayWindow readTlv(
      final ByteArrayWindow window,
      final OnTlv<T> onTlv,
      final T[] result) {
    final int firstTagByte = window.get(0);
    
    final TagClass tagClass = TagClass.fromTagByte(firstTagByte);
    final TagForm tagForm = TagForm.fromTagByte(firstTagByte);
    final long[] tagNo = new long[1];
    final ByteArrayWindow windowPostTagFirst = window.from(1);
    final ByteArrayWindow windowPostTagNo = readTagNo(firstTagByte, windowPostTagFirst, tagNo);
    final int[] tagLength = new int[1];
    final ByteArrayWindow windowPostLength = readTagLength(windowPostTagNo, tagLength);
    final ByteArrayWindow childWindow = windowPostLength.until(tagLength[0]);
    final ByteArrayWindow tagWindow = window.until(windowPostTagNo.start - window.start);
    final ByteArrayWindow lengthWindow = window.until(childWindow.start - windowPostTagNo.start);
    
    onTlv.call(tagWindow, tagClass, tagForm, tagNo[0], lengthWindow, tagLength[0], childWindow, result);
    
    return windowPostLength.from(tagLength[0]);
  }
  
  private static ByteArrayWindow readTagNo(
      final int firstTagByte,
      final ByteArrayWindow window,
      final long[] result) {
    final long tagNoPart = firstTagByte & 0x1f;
    
    if (tagNoPart < 32) {
      result[0] = tagNoPart;
      return window;
    } else {
      int accumulatingTagNo = 0;
      
      ByteArrayWindow nextWindow = window;
      
      while (true) {
        final int nextTagByte = window.get(0);
        nextWindow = window.from(1);
        
        accumulatingTagNo = (accumulatingTagNo << 7) | (nextTagByte & 0x7f);
        
        if ((nextTagByte & 0x80) == 0) {
          break;
        }
      }
      
      result[0] = accumulatingTagNo;
      return nextWindow;
    }
  }
  
  public static ByteArrayWindow readTagLength(final ByteArrayWindow window, int[] length) {
    final int firstLengthByte = window.get(0);
    
    if ((firstLengthByte & 0x80) == 0) {
      length[0] = firstLengthByte & 0x7f;
      return window.from(1);
    } else {
      ByteArrayWindow nextWindow = window.from(1);
      
      final int lengthLength = firstLengthByte & 0x7f;
      int accumulatingLength = 0;
      
      for (int i = 0; i < lengthLength; ++i) {
        accumulatingLength = (accumulatingLength << 8) | (nextWindow.get(0) & 0xff);
        nextWindow = nextWindow.from(1);
      }
      
      length[0] = accumulatingLength;
      return nextWindow;
    }
  }
}
