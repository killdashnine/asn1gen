package org.asn1gen.runtime.java;

import java.util.Iterator;

public class Some<T> implements Option<T> {
  public final T value;
  
  public Some(final T value) {
    this.value = value;
  }
  
  @Override
  public T value() {
    return this.value;
  }

  @Override
  public boolean empty() {
    return false;
  }

  @Override
  public Iterator<T> iterator() {
    return new Iterator<T>() {
      private Some<T> some = Some.this;
      
      @Override
      public boolean hasNext() {
        return some != null;
      }

      @Override
      public T next() {
        if (some == null) {
          throw new RuntimeException();
        }
        
        final T value = some.value;
        some = null;
        return value;
      }

      @Override
      public void remove() {
        throw new RuntimeException();
      }
    };
  }
}
