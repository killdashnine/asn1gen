package org.asn1gen.runtime.java;

public class Statics {
  public static <T> Some<T> some(final T value) {
    return new Some<T>(value);
  }
}
