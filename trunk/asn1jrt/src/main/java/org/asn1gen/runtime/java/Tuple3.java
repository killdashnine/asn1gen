package org.asn1gen.runtime.java;

public class Tuple3<A, B, C> {
  public final A a;
  public final B b;
  public final C c;
  
  public Tuple3(final A a, final B b, final C c) {
    this.a = a;
    this.b = b;
    this.c = c;
  }
}