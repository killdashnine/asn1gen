package org.asn1gen.runtime.java;

public abstract class AsnChoice implements AsnType {
  public abstract AsnType element();

  public abstract int choiceId();
}
