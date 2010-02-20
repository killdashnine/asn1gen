package org.asn1gen.gen

import java.io.File
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
    val outdir = new File("../asn1tuple/src/scala")
    outdir.mkdirs
    
    for (i <- start to end) {
      val functionFile = outdir("Function" + i + ".scala")
      functionFile.withIndentWriter { out =>
        out.println("package scala")
        out.println
        out.println("/**")
        out.println(" * <p>")
        out.println(" * Function with " + i + " parameters.")
        out.println(" * </p>")
        out.println(" */")
        out.print("trait Function" + i + "[")
        for (j <- 1 to i) {
          out.print("-T" + j + ", ")
        }
        out.println("+R] extends AnyRef { self =>")
        out.indent {
          out.print("def apply(")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("v" + j + ": T" + j)
          }
          out.println("): R")
          out.println("override def toString() = \"<function" + i + ">\"")
          out.println
          out.println("/**")
          out.print(" * f(")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("x" + j)
          }
          out.print(")  == (f.curried)")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("(x" + j + ")")
          }
          out.println
          out.println(" */")
          out.print("def curried: ")
          for (j <- 1 to i) {
            out.print("T" + j + " => ")
          } 
          out.println("R = {")
          out.indent {
            for (j <- 1 to i) {
              out.print("(x" + j + ": T" + j + ") => ")
            } 
            out.print("apply(")
            for (j <- 1 to i) {
              if (j != 1) {
                out.print(", ")
              }
              out.print("x" + j)
            }
            out.print(")")
          }
          out.println
          out.println("}")
          out.println
          out.println("/**")
          out.print(" * f(")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("x" + j)
          }
          out.print(") == (f.tupled)(Tuple" + i + "(")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("x" + j)
          }
          out.println("))")
          out.println(" */")
          out.print("def tupled: Tuple" + i + "[")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("T" + j)
          }
          out.println("] => R = {")
          out.indent {
            out.print("case Tuple" + i + "(")
            for (j <- 1 to i) {
              if (j != 1) {
                out.print(", ")
              }
              out.print("x" + j)
            }
            out.print(") => apply(")
            for (j <- 1 to i) {
              if (j != 1) {
                out.print(", ")
              }
              out.print("x" + j)
            }
            out.println(")")
          }
          out.println("}")
        }
        out.println("}")
      }
    
      val productFile = outdir("Produce" + i + ".scala")
      productFile.withIndentWriter { out =>
        out.println("package scala")
        out.println
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
      }
      val tupleFile = outdir("Tuple" + i + ".scala")
      tupleFile.withIndentWriter { out =>
        out.println("package scala")
        out.println
        out.println("import scala.collection.{TraversableLike, IterableLike}")
        out.println("import scala.collection.generic.CanBuildFrom")
        out.println
        out.println("/**")
        out.println(" * Product" + i + " is a cartesian product of " + i + " components.")
        out.println(" * ")
        out.println(" * @since 999.999")
        out.println(" */")
        out.print("trait Product" + i + "[")
        genN(out, "+T", i)
        out.println("] extends Product {")
        out.indent {
          out.println("/**")
          out.println(" * The arity of this product.")
          out.println(" * @return " + i)
          out.println(" */")
          out.println("override def productArity = " + i)
          out.println
          out.println("/**")
          out.println(" * Returns the n-th projection of this product if 0&lt;=n&lt;arity,")
          out.println(" * otherwise null.")
          out.println(" *")
          out.println(" * @param n number of the projection to be returned ")
          out.println(" * @return same as _(n+1)")
          out.println(" * @throws IndexOutOfBoundsException")
          out.println(" */")
          out.println("@throws(classOf[IndexOutOfBoundsException])")
          out.println("override def productElement(n: Int) = n match { ")
          out.indent {
            for (j <- 1 to i) {
              out.println("case " + (j - 1) + " => _" + j)
            }
            out.println("case _ => throw new IndexOutOfBoundsException(n.toString())")
          }
          out.println("}")
          for (j <- 1 to i) {
            out.println
            out.println("/** projection of this product */")
            out.println("def _" + j + ": T" + j)
          }
        }
        out.println("}")
        out.println
        out.println("/**")
        out.println(" * Tuple" + i + " is the canonical representation of a @see Product" + i)
        out.println(" */")
        out.print("case class Tuple" + i + "[")
        genN(out, "+T", i)
        out.print("](")
        for (j <- 1 to i) {
          if (j != 1) {
            out.print(", ")
          }
          out.print("_" + j + ": T" + j)
        }
        out.println(")")
        out.indent {
          out.print("extends Product" + i + "[")
          genN(out, "T", i)
          out.println("]")
        }
        out.println("{")
        out.indent {
          out.print("override def toString() = \"(")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("\" + _" + j + " + \"")
          }
          out.println(")\"")
          out.println
          if (i == 2) {
            out.println("/** Swap the elements of the tuple */")
            out.print("def swap: Tuple" + i + "[")
            genN(out, "T", i)
            out.print("] = Tuple" + i + "(")
            for (j <- i.to(1, -1)) {
              if (j == i) {
                out.print(", ")
              }
              out.print("_")
              out.print(j)
            }
            out.println(")")
            out.println
          }
          out.println("/** Reverse the elements of the tuple */")
          out.print("def reverse: Tuple" + i + "[")
          for (j <- i.to(1, -1)) {
            if (j != i) {
              out.print(", ")
            }
            out.print("T")
            out.print(j)
          }
          out.print("] = Tuple" + i + "(")
          for (j <- i.to(1, -1)) {
            if (j != i) {
              out.print(", ")
            }
            out.print("_")
            out.print(j)
          }
          out.println(")")
          out.println
          out.print("def zip[Repr1, ")
          genN(out, "El", i)
          out.println(", To](")
          out.indent {
            out.indent {
              out.println("implicit w1: T1 => TraversableLike[El1, Repr1],")
              for (j <- 2 to i) {
                out.println("w" + j + ": T" + j + " => Iterable[El" + j + "],")
              }
              out.print("cbf1: CanBuildFrom[Repr1, (")
              genN(out, "El", i)
              out.println("), To]): To = {")
            }
            out.println("val coll1: TraversableLike[El1, Repr1] = _1")
            for (j <- 2 to i) {
              out.println("val coll" + j + ": Iterable[El" + j + "] = _" + j)
            }
            out.println("val b1 = cbf1(coll1.repr)")
            for (j <- 2 to i) {
              out.println("val elems" + j + " = coll" + j + ".iterator")
            }
            out.println
            out.println("for (el1 <- coll1) {")
            out.indent {
              out.print("if (")
              for (j <- 2 to i) {
                if (j != 2) {
                  out.print(" && ")
                }
                out.print("elems" + j + ".hasNext")
              }
              out.println(") {")
              out.indent {
                out.print("b1 += ((el1, ")
                for (j <- 2 to i) {
                  if (j != 2) {
                    out.print(", ")
                  }
                  out.print("elems" + j + ".next")
                }
                out.println("))")
              }
              out.println("}")
            }
            out.println("}")
            out.println
            out.println("b1.result")
          }
          out.println("}")
          out.println
          out.print("def zipped[")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("Repr" + j + ", El" + j)
          }
          out.println("](")
          out.indent {
            out.indent {
              out.print("implicit w1: T1 => TraversableLike[El1, Repr1]")
              for (j <- 2 to i) {
                out.println(",")
                out.print("w" + j + ": T" + j + " => IterableLike[El" + j + ", Repr" + j + "]")
              }
            }
          }
          out.print("): Zipped[")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("Repr" + j + ", El" + j)
          }
          out.println("]")
          out.indent {
            out.print("= new Zipped[")
            for (j <- 1 to i) {
              if (j != 1) {
                out.print(", ")
              }
              out.print("Repr" + j + ", El" + j)
            }
            out.print("](")
            genN(out, "_", i)
            out.println(")")
          }
          out.println
          out.print("class Zipped[")
          out.indent {
            for (j <- 1 to i) {
              if (j != 1) {
                out.print(", ")
              }
              out.print("+Repr" + j + ", +El" + j)
            }
          }
          out.println("](")
          out.indent {
            out.indent {
              out.print("coll1: TraversableLike[El1, Repr1]")
              for (j <- 2 to i) {
                out.println(",")
                out.print("coll" + j + ": IterableLike[El" + j + ", Repr" + j + "]")
              }
              out.println(") {")
            }
            out.print("def map[B, To](f: (")
            genN(out, "El", i)
            out.println(") => B)(implicit cbf: CanBuildFrom[Repr1, B, To]): To = {")
            out.indent {
              out.println("val b = cbf(coll1.repr)")
              for (j <- 2 to i) {
                out.println("val elems" + j + " = coll" + j + ".iterator")
              }
              out.println
              out.println("for (el1 <- coll1) {")
              out.indent {
                out.print("if (")
                for (j <- 2 to i) {
                  if (j != 2) {
                    out.print(" && ")
                  }
                  out.print("elems" + j + ".hasNext")
                }
                out.println(") {")
                out.indent {
                  out.print("b += f(el1, ")
                  for (j <- 2 to i) {
                    if (j != 2) {
                      out.print(", ")
                    }
                    out.print("elems" + j + ".next")
                  }
                  out.println(")")
                }
                out.println("}")
              }
              out.println("}")
              out.println
              out.println("b.result")
            }
            out.println("}")
            out.println
            out.print("def flatMap[B, To](f: (")
            genN(out, "El", i)
            out.println(") => Traversable[B])(implicit cbf: CanBuildFrom[Repr1, B, To]): To = {")
            out.indent {
              out.println("val b = cbf(coll1.repr)")
              for (j <- 2 to i) {
                out.println("val elems" + j + " = coll" + j + ".iterator")
              }
              out.println
              out.println("for(el1 <- coll1)")
              out.indent {
                out.print("if(")
                for (j <- 2 to i) {
                  if (j != 2) {
                    out.print(" && ")
                  }
                  out.print("elems" + j + ".hasNext")
                }
                out.println(") {")
                out.indent {
                  out.print("b ++= f(el1")
                  for (j <- 2 to i) {
                    out.print(", elems" + j + ".next")
                  }
                  out.println(")")
                }
                out.println("}")
              }
              out.println
              out.println("b.result")
            }
            out.println("}")
            out.println
            out.print("def filter[")
            genN(out, "To", i)
            out.print("](f: (")
            genN(out, "El", i)
            out.println(") => Boolean)(")
            out.indent {
              out.indent {
                out.print("implicit cbf1: CanBuildFrom[Repr1, El1, To1]")
                for (j <- 2 to i) {
                  out.println(",")
                  out.print("cbf" + j + ": CanBuildFrom[Repr" + j + ", El" + j + ", To" + j + "]")
                }
                out.print("): (")
                genN(out, "To", i)
                out.println(") = {")
              }
              for (j <- 1 to i) {
                out.println("val b" + j + " = cbf" + j + "(coll" + j + ".repr)")
              }
              for (j <- 2 to i) {
                out.println("val elems" + j + " = coll" + j + ".iterator")
              }
              out.println
              out.println("for (el1 <- coll1) {")
              out.indent {
                out.print("if (")
                for (j <- 2 to i) {
                  if (j != 2) {
                    out.print(" && ")
                  }
                  out.print("elems" + j + ".hasNext")
                }
                out.println(") {")
                out.indent {
                  for (j <- 2 to i) {
                    out.println("val el" + j + " = elems" + j + ".next")
                  }
                  out.print("if (f(")
                  genN(out, "el", i)
                  out.println(")) {")
                  out.indent {
                    for (j <- 1 to i) {
                      out.println("b" + j + " += el" + j)
                    }
                  }
                  out.println("}")
                }
                out.println("}")
              }
              out.println("}")
              out.println
              out.print("(")
              for (j <- 1 to i) {
                if (j != 1) {
                  out.print(", ")
                }
                out.print("b" + j + ".result")
              }
              out.println(")")
            }
            out.println("}")
            out.println
            out.print("def exists(f: (")
            genN(out, "El", i)
            out.println(") => Boolean): Boolean = {")
            out.indent {
              out.println("var acc = false")
              for (j <- 2 to i) {
                out.println("val elems" + j + " = coll" + j + ".iterator")
              }
              out.println
              out.println("for (el1 <- coll1) {")
              out.indent {
                out.print("if (!acc")
                for (j <- 2 to i) {
                  out.print(" && elems" + j + ".hasNext")
                }
                out.println(") {")
                out.indent {
                  out.print("acc = f(el1")
                  for (j <- 2 to i) {
                    out.print(", elems" + j + ".next")
                  }
                  out.println(")")
                }
                out.println("}")
              }
              out.println("}")
              out.println
              out.println("acc")
            }
            out.println("}")
            out.println
            out.print("def forall(f: (")
            genN(out, "El", i)
            out.println(") => Boolean): Boolean = {")
            out.indent {
              out.println("var acc = true")
              for (j <- 2 to i) {
                out.println("val elems" + j + " = coll" + j + ".iterator")
              }
              out.println
              out.println("for (el1 <- coll1) {")
              out.indent {
                out.print("if (acc")
                for (j <- 2 to i) {
                  out.print(" && elems" + j + ".hasNext")
                }
                out.println(") {")
                out.indent {
                  out.print("acc = f(el1")
                  for (j <- 2 to i) {
                    out.print(", elems" + j + ".next")
                  }
                  out.println(")")
                }
                out.println("}")
              }
              out.println("}")
              out.println
              out.println("acc")
            }
            out.println("}")
            out.println
            out.print("def foreach[U](f: (")
            genN(out, "El", i)
            out.println(") => U): Unit = {")
            out.indent {
              for (j <- 2 to i) {
                out.println("val elems" + j + " = coll" + j + ".iterator")
              }
              out.println
              out.println("for (el1 <- coll1) {")
              out.indent {
                out.print("if (")
                for (j <- 2 to i) {
                  if (j != 2) {
                    out.print(" && ")
                  }
                  out.print("elems" + j + ".hasNext")
                }
                out.println(") {")
                out.indent {
                  out.print("f(el1")
                  for (j <- 2 to i) {
                    out.print(", elems" + j + ".next")
                  }
                  out.println(")")
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
