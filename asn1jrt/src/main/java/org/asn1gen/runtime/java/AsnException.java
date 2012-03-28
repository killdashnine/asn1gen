package org.asn1gen.runtime.java;

public class AsnException extends Exception {
  private static final long serialVersionUID = 8580046513023395815L;

  public AsnException() {
    super();
  }

  public AsnException(final String message) {
    super(message);
  }

  public AsnException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public AsnException(Throwable cause) {
    super(cause);
  }

  protected AsnException(
      final String message,
      final Throwable cause,
      final boolean enableSuppression,
      final boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }
}
