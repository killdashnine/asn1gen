package org.asn1gen.runtime.java;

import java.io.PrintStream;
import java.io.PrintWriter;

public class TLV {
  public static void dump(final PrintStream out, final ByteArrayWindow window) {
    try (final IndentWriter indentWriter = new IndentWriter(out)) {
      dump(indentWriter, window);
    }
  }

  public static void dump(final PrintWriter out, final ByteArrayWindow window) {
    try (final IndentWriter indentWriter = new IndentWriter(out)) {
      dump(indentWriter, window);
    }
  }

  public static ByteArrayWindow dump(final IndentWriter out, final ByteArrayWindow window) {
    if (window.length == 0) {
      return window;
    }
    
    final Tuple5<TagClass, TagForm, Integer, ByteArrayWindow, ByteArrayWindow> tagTuple = tagOf(window);
    final ByteArrayWindow tagWindow = tagTuple.d;
    final ByteArrayWindow afterTagWindow = tagTuple.e;
    final TagClass tagClass = tagTuple.a;
    final TagForm tagForm = tagTuple.b;
    final int tagNo = tagTuple.c;
    final Tuple3<Integer, ByteArrayWindow, ByteArrayWindow> lengthTuple = lengthOf(afterTagWindow);
    final int length = lengthTuple.a;
    final ByteArrayWindow lengthWindow = lengthTuple.b;
    final ByteArrayWindow afterLengthWindow = lengthTuple.c;
    final ByteArrayWindow dataWindow = afterLengthWindow.until(length);
    final ByteArrayWindow remainderWindow = afterLengthWindow.from(length);
    
    final Delimeter tagDelimeter = new Delimeter("", " ");
    
    out.hex(tagWindow).$(' ').$('[').$(tagDelimeter).$(tagClass).$(tagDelimeter).$(tagForm).$(tagDelimeter).$(tagNo).$("]").$(' ');
    out.hex(lengthWindow).$(' ').$('[').$(length).$(']');
    
    if (tagForm == TagForm.PRIMITIVE) {
      switch (tagNo) {
      case 1:
        assert length == 1;
        out.$(' ').hex(dataWindow).$(' ');
        out.$("[BOOLEAN:").$(dataWindow.get(0) != 0 ? "true" : "false").$("]");
        break;
      case 4: // Octet String
        assert length == 1;
        out.$(' ').hex(dataWindow).$(dataWindow.length > 0 ? " " : "");
        out.$("[OCTET_STRING]");
        break;
      case 10:
        assert length == 1;
        out.$(' ').hex(dataWindow).$(' ');
        out.$("[ENUMERATION:").$(intValue(dataWindow)).$("]");
        break;
      }
      out.endln();
    } else {
      if (dataWindow.length > 0) {
        out.$(" {").endln();
        
        try (final Indent indent = out.indent(2)) {
          switch (tagClass) {
          case UNIVERSAL:
            switch (tagForm) {
            case PRIMITIVE:
            case CONSTRUCTED:
              if (true) {
                ByteArrayWindow childWindow = dataWindow;
                while (childWindow.length > 0) {
                  childWindow = dump(out, childWindow);
                }
                break;
              }
            }
            break;
          default:
            out.hex(dataWindow).endln();
            break;
          }
        }
        
        out.$("}").endln();
      } else {
        out.endln();
      }
    }
    
    return remainderWindow;
  }
  
  private static int intValue(final ByteArrayWindow dataWindow) {
    if (dataWindow.length == 0) {
      return 0;
    } else {
      return dataWindow.get(0);
    }
  }

  public static Tuple5<TagClass, TagForm, Integer, ByteArrayWindow, ByteArrayWindow> tagOf(final ByteArrayWindow window) {
    final byte tagByte = window.get(0);
    final TagClass tagClass = TagClass.fromTagByte(tagByte);
    final TagForm tagForm = TagForm.fromTagByte(tagByte);
    final int shortTag = tagByte & 0x1f;
    
    if (shortTag < 32) {
      return new Tuple5<TagClass, TagForm, Integer, ByteArrayWindow, ByteArrayWindow>(
          tagClass, tagForm, shortTag, window.until(1), window.from(1));
    }
    
    return tagOf(tagClass, tagForm, 0, window, window.from(1));
  }
    
  public static Tuple5<TagClass, TagForm, Integer, ByteArrayWindow, ByteArrayWindow> tagOf(
      final TagClass tagClass,
      final TagForm tagForm,
      final int accumulator,
      final ByteArrayWindow tagStartWindow,
      final ByteArrayWindow window) {
    final int value = (accumulator << 7) + (window.get(0) & (int)0x7f);
    final ByteArrayWindow nextWindow = window.from(1);
    
    if ((window.get(0) & 0x80) != 0) {
      return tagOf(tagClass, tagForm, value, tagStartWindow, nextWindow);
    }
    
    return new Tuple5<TagClass, TagForm, Integer, ByteArrayWindow, ByteArrayWindow>(
        tagClass, tagForm, value, tagStartWindow.until(nextWindow), nextWindow);
  }
  
  public static Tuple3<Integer, ByteArrayWindow, ByteArrayWindow> lengthOf(final ByteArrayWindow window) {
    return lengthOf(window, window);
  }
  
  public static Tuple3<Integer, ByteArrayWindow, ByteArrayWindow> lengthOf(final ByteArrayWindow startWindow, final ByteArrayWindow window) {
    final byte lengthByte = window.get(0);
    
    if ((lengthByte & 0x80) == 0) {
      final int shortLength = lengthByte;
      
      return new Tuple3<Integer, ByteArrayWindow, ByteArrayWindow>(shortLength, window.until(1), window.from(1));
    } else {
      final int lengthLength = lengthByte & 0x7f;
      final ByteArrayWindow workingWindow = window.from(1);
      int length = 0;
      
      for (int i = 0; i < lengthLength; ++i) {
        length = (length << 8) | window.get(i);
      }
      
      return new Tuple3<Integer, ByteArrayWindow, ByteArrayWindow>(length, workingWindow.until(lengthLength), workingWindow.from(lengthLength));
    }
  }
  
  public static void main(final String[] args) {
    dump(System.out, ByteArrayWindow.to(new byte[] { 0x31, 0x0c, 0x04, 0x00, 0x04, 0x00, 0x04, 0x00, 0x0a, 0x01, 0x00, 0x01, 0x01, 0x00 }));
  }
}
