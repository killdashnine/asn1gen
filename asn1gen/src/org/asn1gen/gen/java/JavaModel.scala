package org.asn1gen.gen.java

import java.io.File
import org.asn1gen.extra.Extras._
import org.asn1gen.io.IndentWriter
import org.asn1gen.parsing.asn1.Asn1Parser
import org.asn1gen.parsing.asn1.{ast => ast}
import scala.collection.immutable._
import scala.io.Source

case class JavaModel(
    modules: HashMap[String, Module], 
    namespace: String = "", 
    pathOut: File = new File(".")) extends Asn1Parser {
  lazy val namespacePath = pathOut / namespace.replaceAll(".", "/")
  lazy val pathModel = namespacePath / "model"
  lazy val pathMeta = namespacePath / "meta"
  lazy val pathCodec = namespacePath / "codec"
  
  def parse[N](root: Parser[N], input: String) =
    phrase(root)(new lexical.Scanner(input))
  
  def generate(): Unit = {
    modules foreach { case (moduleName, module) =>
      val modulePath = (pathModel / moduleName).make
      val genJava = new GenJava(this, modulePath, moduleName)
      genJava.generate(module)
    }
  }
  
  def load(file: File): JavaModel = {
    val text = Source.fromFile(file).mkString
    parse(root, text) match {
      case Success(moduleDefinition, _) =>
        val refactoredModuleDefinition = AnonymousTypeNamer.process(moduleDefinition)
        //GenJavaAst.generate(new IndentWriter(System.out), refactoredModuleDefinition)
        val name = refactoredModuleDefinition.name
        if (modules.contains(name)) {
          throw new ModuleLoadException("Module " + name + " already exists")
        }
        JavaModel(modules + (refactoredModuleDefinition.name -> Module.from(refactoredModuleDefinition)))
      case failure =>
        throw new ModuleLoadException("Parse failure: " + failure)
    }
  }
  
  def genJava(file: File): Unit = {
    file.mkdirs
    modules foreach { module =>
    }
  }
  
  def write(): Unit = {
    modules foreach { case (moduleName, module) =>
      val genJava = new GenJava(this, pathModel.make, moduleName)
      genJava.generate(module)
    }
    modules foreach { case (moduleName, module) =>
      val moduleFile = pathMeta.make.child(moduleName + ".java")
      moduleFile.openPrintStream { ps =>
        val genJava = new GenJavaMeta("moo", new IndentWriter(ps))
        genJava.generate(module)
        println("Writing to " + moduleFile)
      }
    }
    modules foreach { case (moduleName, module) =>
      val moduleFile = pathCodec.make.child(moduleName + ".java")
      moduleFile.withIndentWriter { out =>
        val genJava = new GenJavaBerEncoder("moo", out)
        genJava.generate(module)
        println("Writing to " + moduleFile)
      }
    }
  }
}

object JavaModel {
  def empty = JavaModel(HashMap[String, Module]())
}
