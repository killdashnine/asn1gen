package org.asn1gen.runtime.java;

import java.io.PrintStream;

public class TLV {
  public static void dump(final PrintStream out, final ByteArrayWindow window) {
    try (final IndentWriter indentWriter = new IndentWriter(out)) {
      dump(indentWriter, window);
    }
  }
  
  public static ByteArrayWindow dump(final IndentWriter out, final ByteArrayWindow window) {
    Void[] result = new Void[1];
    
    return readTlv(
        window,
        new OnTlv<Void>() {
          @Override
          public ByteArrayWindow call(
              final ByteArrayWindow tagWindow,
              final TagClass tagClass,
              final TagForm tagForm,
              final long tagNo,
              final ByteArrayWindow lengthWindow,
              final int length,
              final ByteArrayWindow valueWindow,
              final Void[] result) {
            final Delimeter tagDelimeter = new Delimeter("", " ");
            
            out.hex(tagWindow).$(' ').$('[').$(tagDelimeter).$(tagClass).$(tagDelimeter).$(tagForm).$(tagDelimeter).$(tagNo).$("]").$(' ');
            out.hex(lengthWindow).$(' ').$('[').$(length).$(']');
            
            if (tagForm == TagForm.PRIMITIVE) {
              if (tagNo == 1) {
                assert length == 1;
                out.$(' ').hex(valueWindow).$(' ');
                out.$("[BOOLEAN:").$(valueWindow.get(0) != 0 ? "true" : "false").$("]");
              } else if (tagNo == 4) { // Octet String
                assert length == 1;
                out.$(' ').hex(valueWindow).$(valueWindow.length > 0 ? " " : "");
                out.$("[OCTET_STRING]");
              } else if (tagNo == 10) {
                assert length == 1;
                out.$(' ').hex(valueWindow).$(' ');
                out.$("[ENUMERATION:").$(intValue(valueWindow)).$("]");
              }
              out.endln();
            } else {
              if (valueWindow.length > 0) {
                out.$(" {").endln();
                
                try (final Indent indent = out.indent(2)) {
                  switch (tagClass) {
                  case UNIVERSAL:
                    switch (tagForm) {
                    case PRIMITIVE:
                    case CONSTRUCTED:
                      if (true) {
                        ByteArrayWindow childWindow = valueWindow;
                        while (childWindow.length > 0) {
                          childWindow = dump(out, childWindow);
                        }
                        break;
                      }
                    }
                    break;
                  default:
                    out.hex(valueWindow).endln();
                    break;
                  }
                }
                
                out.$("}").endln();
              } else {
                out.endln();
              }
            }
            
            return null;
          }
        },
        result);
  }
  
  private static int intValue(final ByteArrayWindow dataWindow) {
    if (dataWindow.length == 0) {
      return 0;
    } else {
      return dataWindow.get(0);
    }
  }

  public static void main(final String[] args) {
    dump(System.out, ByteArrayWindow.to(new byte[] { 0x31, 0x0c, 0x04, 0x00, 0x04, 0x00, 0x04, 0x00, 0x0a, 0x01, 0x00, 0x01, 0x01, 0x00 }));
  }

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
