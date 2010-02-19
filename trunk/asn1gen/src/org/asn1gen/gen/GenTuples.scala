package org.asn1gen.gen

import org.asn1gen.extra.Extras._
import org.asn1gen.io._

object GenTuples {
  def genN(out: IndentWriter, prefix: String, count: Int) = {
    for (i <- 1 to count) {
      if (i != 1) {
        out.print(", ")
      }
      out.print(prefix)
      out.print(i)
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val start: Int = java.lang.Integer.parseInt(args(0))
    val end: Int = java.lang.Integer.parseInt(args(1))
    
    System.out.withIndentWriter { out =>
      out.println("package scala")
      out.println
      out.println("import scala.collection.{TraversableLike, IterableLike}")
      out.println("import scala.collection.generic.CanBuildFrom")
      out.println
      for (i <- start to end) {
        out.println("object Product" + i + " {")
        out.indent {
          out.print("def unapply[")
          genN(out, "T", i)
          out.print("](x: Product" + i + "[")
          genN(out, "T", i)
          out.print("]): Option[Product" + i + "[")
          genN(out, "T", i)
          out.println("]] =")
          out.indent {
            out.println("Some(x)")
          }
        }
        out.println("}")
        out.println("")
        out.println("/** Product" + i + " is a cartesian product of " + i + " components.")
        out.println(" *  ")
        out.println(" *  @since 999.999")
        out.println(" */")
        out.print("trait Product" + i + "[")
        genN(out, "+T", i)
        out.println("] extends Product {")
        out.indent {
          out.println("/**")
          out.println(" *  The arity of this product.")
          out.println(" *  @return " + i)
          out.println(" */")
          out.println("override def productArity = " + i)
          out.println("")
          out.println("/**")
          out.println(" *  Returns the n-th projection of this product if 0&lt;=n&lt;arity,")
          out.println(" *  otherwise null.")
          out.println(" *")
          out.println(" *  @param n number of the projection to be returned ")
          out.println(" *  @return  same as _(n+1)")
          out.println(" *  @throws  IndexOutOfBoundsException")
          out.println(" */")
          out.println("@throws(classOf[IndexOutOfBoundsException])")
          out.println("override def productElement(n: Int) = n match { ")
          out.indent {
            out.println("case 0 => _1")
            out.println("case 1 => _2")
            out.println("case _ => throw new IndexOutOfBoundsException(n.toString())")
          }
          out.println("}")
          out.println("")
          out.println("/** projection of this product */")
          out.println("def _1: T1")
          out.println("")
          out.println("/** projection of this product */")
          out.println("def _2: T2")
        }
        out.println("}")
        out.println("")
        out.println
        out.println("/** Tuple" + i + " is the canonical representation of a @see Product" + i)
        out.println(" *")
        out.println(" */")
        out.println("case class Tuple" + i + "[+T1, +T2](_1:T1,_2:T2)")
        out.indent {
          out.println("extends Product" + i + "[T1, T2]")
        }
        out.println("{  ")
        out.indent {
          out.println("override def toString() = \"(\" + _1 + \",\" + _2 + \")\"")
          out.println("")
          out.println("/** Swap the elements of the tuple */")
          out.println("def swap: Tuple" + i + "[T2,T1] = Tuple" + i + "(_2, _1)")
          out.println
          out.println("def zip[Repr1, El1, El2, To](")
          out.indent {
            out.indent {
              out.println("implicit w1:   T1 => TraversableLike[El1, Repr1],")
              out.println("w2:   T2 => Iterable[El2],")
              out.println("cbf1: CanBuildFrom[Repr1, (El1, El2), To]): To = {")
            }
            out.println("val coll1: TraversableLike[El1, Repr1] = _1")
            out.println("val coll2: Iterable[El2] = _2")
            out.println("val b1 = cbf1(coll1.repr)")
            out.println("val elems" + i + " = coll" + i + ".iterator")
            out.println
            out.println("for (el1 <- coll1) {")
            out.indent {
              out.println("if (elems" + i + ".hasNext) {")
              out.indent {
                out.println("b1 += ((el1, elems" + i + ".next))")
              }
              out.println("}")
            }
            out.println("}")
            out.println
            out.println("b1.result")
          }
          out.println("}")
          out.println
          out.println("def zipped[Repr1, El1, Repr2, El2](implicit w1: T1 => TraversableLike[El1, Repr1], w2: T2 => IterableLike[El2, Repr2]): Zipped[Repr1, El1, Repr2, El2]")
          out.indent {
            out.println("= new Zipped[Repr1, El1, Repr2, El2](_1, _2)")
          }
          out.println
          out.println("class Zipped[+Repr1, +El1, +Repr2, +El2](coll1: TraversableLike[El1, Repr1], coll2: IterableLike[El2, Repr2]) { // coll2: IterableLike for filter")
          out.indent {
            out.println("def map[B, To](f: (El1, El2) => B)(implicit cbf: CanBuildFrom[Repr1, B, To]): To = {")
            out.indent {
              out.println("val b = cbf(coll1.repr)")
              out.println("val elems" + i + " = coll" + i + ".iterator")
              out.println
              out.println("for (el1 <- coll1) {")
              out.indent {
                out.println("if (elems" + i + ".hasNext) {")
                out.indent {
                  out.println("b += f(el1, elems" + i + ".next)")
                }
                out.println("}")
              }
              out.println("}")
              out.println
              out.println("b.result")
            }
            out.println("}")
            out.println
            out.println("def flatMap[B, To](f: (El1, El2) => Traversable[B])(implicit cbf: CanBuildFrom[Repr1, B, To]): To = {")
            out.indent {
              out.println("val b = cbf(coll1.repr)")
              out.println("val elems" + i + " = coll" + i + ".iterator")
              out.println
              out.println("for(el1 <- coll1)")
              out.indent {
                out.println("if(elems" + i + ".hasNext) {")
                out.indent {
                  out.println("b ++= f(el1, elems" + i + ".next)")
                }
                out.println("}")
              }
              out.println
              out.println("b.result")
            }
            out.println("}")
            out.println
            out.println("def filter[To1, To2](f: (El1, El2) => Boolean)(implicit cbf1: CanBuildFrom[Repr1, El1, To1], cbf2: CanBuildFrom[Repr2, El2, To2]): (To1, To2) = {")
            out.indent {
              out.println("val b1 = cbf1(coll1.repr)")
              out.println("val b2 = cbf2(coll2.repr)")
              out.println("val elems" + i + " = coll" + i + ".iterator")
              out.println
              out.println("for (el1 <- coll1) {")
              out.indent {
                out.println("if (elems" + i + ".hasNext) {")
                out.indent {
                  out.println("val el" + i + " = elems" + i + ".next")
                  out.println("if (f(el1, el2)) {")
                  out.indent {
                    out.println("b1 += el1")
                    out.println("b2 += el2")
                  }
                  out.println("}")
                }
                out.println("}")
              }
              out.println("}")
              out.println
              out.println("(b1.result, b2.result)")
            }
            out.println("}")
            out.println
            out.println("def exists(f: (El1, El2) => Boolean): Boolean = {")
            out.indent {
              out.println("var acc = false")
              out.println("val elems" + i + " = coll" + i + ".iterator")
              out.println
              out.println("for (el1 <- coll1) {")
              out.indent {
                out.println("if (!acc && elems" + i + ".hasNext) {")
                out.indent {
                  out.println("acc = f(el1, elems" + i + ".next)")
                }
                out.println("}")
              }
              out.println("}")
              out.println
              out.println("acc")
            }
            out.println("}")
            out.println
            out.println("def forall(f: (El1, El2) => Boolean): Boolean = {")
            out.indent {
              out.println("var acc = true")
              out.println("val elems" + i + " = coll" + i + ".iterator")
              out.println
              out.println("for (el1 <- coll1) {")
              out.indent {
                out.println("if (acc && elems" + i + ".hasNext) {")
                out.indent {
                  out.println("acc = f(el1, elems" + i + ".next)")
                }
              }
              out.println("}")
              out.println
              out.println("acc")
            }
            out.println("}")
            out.println
            out.println("def foreach[U](f: (El1, El2) => U): Unit = {")
            out.indent {
              out.println("val elems" + i + " = coll" + i + ".iterator")
              out.println
              out.println("for (el1 <- coll1) {")
              out.indent {
                out.println("if (elems" + i + ".hasNext) {")
                out.indent {
                  out.println("f(el1, elems" + i + ".next)")
                }
                out.println("}")
              }
              out.println("}")
            }
            out.println("}")
          }
          out.println("}")
        }
        out.println("}")
        out.println
      }
    }
  }
}
