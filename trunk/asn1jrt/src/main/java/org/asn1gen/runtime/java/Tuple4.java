package org.asn1gen.runtime.java;

public class Tuple4<A, B, C, D> {
  public final A a;
  public final B b;
  public final C c;
  public final D d;
  
  public Tuple4(final A a, final B b, final C c, final D d) {
    this.a = a;
    this.b = b;
    this.c = c;
    this.d = d;
  }
}