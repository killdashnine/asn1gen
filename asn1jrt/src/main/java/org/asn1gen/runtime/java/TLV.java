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
    
    return TlvReader.readTlv(
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
}
