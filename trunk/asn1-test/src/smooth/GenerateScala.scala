package smooth

import java.io._
import org.asn1gen.parsing.asn1.Asn1Parser
import org.asn1gen.parsing.asn1.ast._
import scala.util.parsing.input._
import scala.io.Source
import org.asn1gen.gen._
import org.asn1gen.gen.scala._
import org.asn1gen.extra.Extras._
import org.asn1gen.io.IndentWriter

object GenerateScala extends Asn1Parser {
  def parse[N](root: Parser[N], input: String) =
    phrase(root)(new lexical.Scanner(input))

  def main(args : Array[String]) : Unit = {
    // Load model
    val inputDirectory = new File("asn")
    inputDirectory.requireExists
    inputDirectory.requireDirectory
    val children = inputDirectory.children { file =>
      file.isFile && file.name.endsWith(".asn1")
    }
    
    var model = (ScalaModel.empty /: children) { (model, child) =>
      println("Loading: " + child.name)
      model.load(child)
    }
    
    model.modules foreach { case (name, module) =>
      module.types foreach { case (name, namedType) =>
        System.out.withIndentWriter { out =>
          out.print(name)
          out.print(" = ")
          GenScalaAst.generate(out, namedType._type)
          out.println()
        }
      }
    }
    
    // Output model
    try {
      val outDirectory = new File("out")
      outDirectory.mkdir
      model.writeTo(outDirectory)
    } catch {
      case e: AsnCodeGenerationException => {
        System.out.print("Error: ")
        System.out.println(e.getMessage)
      }
    }
    
    println("Done.")
  }
}
