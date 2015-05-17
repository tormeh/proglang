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
        println("no file found in arguments\n")
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
        println("too many arguments\n")
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
    FumurtScanner.scan(sourcestring) match
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
                val errornum:String = errors.length match
                {
                  case 1 => "one"
                  case 2 => "two"
                  case 3 => "three"
                  case 4 => "four"
                  case 5 => "five"
                  case 6 => "six"
                  case 7 => "seven"
                  case 8 => "eight"
                  case 9 => "nine"
                  case x => x.toString
                }
                println(errornum.capitalize+" errors found")
              }
              case None => 
              {
                println("\nNo errors in checker")
                val generatedcode = FumurtCodeGenerator.generate(ast)
                println("\ncode generated: \n" + generatedcode)
                import java.nio.file.{Paths, Files}
                import java.nio.charset.StandardCharsets
                val outname = "generated"
                val fileending = ".cpp"
                Files.write(Paths.get("./"+outname+fileending), generatedcode.getBytes(StandardCharsets.UTF_8))
                val options = " -pthread -std=c++11 -O3 -march=native"
                println("\n\n===Starting Clang cpp compilation===")
                //println("options = " + options)
                import scala.sys.process._
                val command = "clang++ " + outname + fileending + options + " -o " + outname
                println(command)
                (command).!
              }
            }
          }
        }
      }
    }
    
  }
  
}

class Options(val compileTypeOption:CompileTypeOption, val debug:Boolean, val file:String)


