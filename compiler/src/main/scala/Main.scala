package fumurtCompiler

import scala.io.Source._
import scala.util.parsing.input.Positional

object CompileTypeOption extends Enumeration
{
  type CompileTypeOption = Value
  val compiledToGo, compiledToC, compiledToCpp, interpreted = Value 
}

import CompileTypeOption._

object Main
{
  def main(args: Array[String]) :Unit ={
    if(args.length <1)
    {
        println("no file found\n")
    }
    else
    {
      val parts = args(0).split("""\.""")
      if(parts.length==2)
      {
        if(parts(1)=="fumurt")
        {
          compile(getOptions(args.drop(1), args(0)))
        }
        else
        {
          println("unknown file ending: " + parts(1) + "\n")
        }
      }
      else
      {
        println("no file found\n")
      }
    }
  }
  
  def getOptions(args:Array[String],file:String): Options =
  {
    println(args.toString)
    new Options(CompileTypeOption.interpreted, true, file)
  }
  
  def compile(opts:Options):Unit =
  {
    println("Now compiling!")
    val sourcestring = fromFile(opts.file).mkString
    FumurtScanner.scan(sourcestring, opts) match
    {
      case Left(error) => println("Error in scanner: " + error.toString)
      case Right(tokens) => 
      {
        println("successful scan. Tokens: "+tokens.toString+"\n")
        FumurtParser.parse(tokens) match
        {
          case Left(error) => println("Error in parser: " + error.toString)
          case Right(ast) => 
          {
            println("Success in parser: " + ast.toString)
            FumurtTypeChecker.check(ast) match
            {
              case Some(errors) => 
              {
                errors.map(x=>println(x))
              }
              case None => 
              {
                println("\nNo errors in checker")
                val generatedcode = FumurtCodeGenerator.generate(ast)
                println("\ncode generated: \n" + generatedcode)
                import java.nio.file.{Paths, Files}
                import java.nio.charset.StandardCharsets
                val outname = "generated.cpp"
                Files.write(Paths.get("./"+outname), generatedcode.getBytes(StandardCharsets.UTF_8))
                val options = " -pthread -std=c++11 -O3"
                println("\n\n===Starting Clang cpp compilation===")
                println("options = " + options)
                import scala.sys.process._
                ("clang " + outname + options).!
              }
            }
          }
        }
      }
    }
    
  }
  
}

class Options(val compileTypeOption:CompileTypeOption, val debug:Boolean, val file:String)


