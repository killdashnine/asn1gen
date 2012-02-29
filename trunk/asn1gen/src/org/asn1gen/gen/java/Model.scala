package org.asn1gen.gen.java

import java.io.File
import org.asn1gen.extra.Extras._
import org.asn1gen.io.IndentWriter
import org.asn1gen.parsing.asn1.Asn1Parser
import org.asn1gen.parsing.asn1.{ast => ast}
import scala.collection.immutable._
import scala.io.Source

case class Model (modules: HashMap[String, Module]) extends Asn1Parser {
  def parse[N](root: Parser[N], input: String) =
    phrase(root)(new lexical.Scanner(input))
  
  def generateTo(packageName: String, directory: File): Unit = {
    modules foreach { case (moduleName, module) =>
      val modulePath = directory.child(moduleName)
      modulePath.mkdir
      val genJava = new GenJava(modulePath, moduleName)
      genJava.generate(module)
    }
  }
  
  def load(file: File): Model = {
    val text = Source.fromFile(file).mkString
    parse(root, text) match {
      case Success(moduleDefinition, _) =>
        val refactoredModuleDefinition = AnonymousTypeNamer.process(moduleDefinition)
        //GenJavaAst.generate(new IndentWriter(System.out), refactoredModuleDefinition)
        val name = refactoredModuleDefinition.name
        if (modules.contains(name)) {
          throw new ModuleLoadException("Module " + name + " already exists")
        }
        Model(modules + (refactoredModuleDefinition.name -> Module.from(refactoredModuleDefinition)))
      case failure =>
        throw new ModuleLoadException("Parse failure: " + failure)
    }
  }
  
  def genJava(file: File): Unit = {
    file.mkdirs
    modules foreach { module =>
    }
  }
  
  def writeTo(outDirectory: File): Unit = {
    outDirectory.mkdir
    val metaDirectory = outDirectory.child("meta")
    metaDirectory.mkdir
    val codecDirectory = outDirectory.child("codec")
    codecDirectory.mkdir
    val berDirectory = codecDirectory.child("ber")
    berDirectory.mkdir
    modules foreach { case (moduleName, module) =>
      val genJava = new GenJava(outDirectory, moduleName)
      genJava.generate(module)
    }
    modules foreach { case (moduleName, module) =>
      val moduleFile = metaDirectory.child(moduleName + ".java")
      moduleFile.openPrintStream { ps =>
        val genJava = new GenJavaMeta("moo", new IndentWriter(ps))
        genJava.generate(module)
        println("Writing to " + moduleFile)
      }
    }
    modules foreach { case (moduleName, module) =>
      val moduleFile = berDirectory.child(moduleName + ".java")
      moduleFile.openPrintStream { ps =>
        val genJava = new GenJavaBerEncoder("moo", new IndentWriter(ps))
        genJava.generate(module)
        println("Writing to " + moduleFile)
      }
    }
  }
}

object Model {
  def empty = Model(HashMap[String, Module]())
}
