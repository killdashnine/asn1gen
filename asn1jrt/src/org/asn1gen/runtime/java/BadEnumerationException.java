package org.asn1gen.runtime.java;

@SuppressWarnings("serial")
public class BadEnumerationException extends RuntimeException {
  public BadEnumerationException() {
  }
  
  public BadEnumerationException(final String message) {
    super(message);
  }

  public BadEnumerationException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public BadEnumerationException(final Throwable cause) {
    super(cause);
  }
}
