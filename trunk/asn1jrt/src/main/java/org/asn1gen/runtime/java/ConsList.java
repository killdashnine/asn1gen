package org.asn1gen.runtime.java;

public abstract class ConsList<T> implements Iterable<T> {
  public abstract boolean empty();
  
  public abstract T value();
  
  public abstract ConsList<T> tail();
  
  public ConsList<T> prepend(final T item) {
    return new Cons<T>(item, this);
  }
  
  public ConsList<T> reverse() {
    ConsList<T> target = Nil.<T>instance();
    
    for (final T item: this) {
      target = target.prepend(item);
    }
    
    return target;
  }
}
