package org.asn1gen.runtime.java;

public class Delimeter {
  private final String firstValue;
  private final String value;
  private boolean isFirst = true;
  
  public Delimeter(final String firstValue, final String value) {
    this.firstValue = firstValue;
    this.value = value;
  }
  
  public IndentWriter writeTo(final IndentWriter out) {
    if (isFirst) {
      isFirst = false;
      return out.$(firstValue);
    } else {
      return out.$(value);
    }
  }
}
