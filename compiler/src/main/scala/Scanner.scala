//https://wiki.scala-lang.org/display/SW/Parser+Combinators--Getting+Started
//http://en.wikipedia.org/wiki/Regular_expression#Syntax


package fumurtCompiler

import scala.language.implicitConversions
import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.util.parsing.combinator.lexical._

object FumurtScanner extends RegexParsers /*with Parsers*/
{
  override val skipWhitespace = false
  
  def scan(in:String, opts:Options):Either[FumurtError, List[Token]] =
  {
    println(in) 
   
    parseAll((scan*), in) match
    {
      case Success(result, _) =>
      {
        val tokens = result.filter(x=>x match{case SpaceT() => false; case _ => true}) :+ EofT()
        Right(tokens)
      }
      case Failure(message, _) => Left(new FumurtError(Global, "Failure: "+message))
      case Error(message,_) => Left(new FumurtError(Global, "Error: " + message))
    }
   
  }
  
  
  def removeSpaces(intokens:List[Token]):List[Token] =
  {
    intokens
  }

  def spaceParser:Parser[SpaceT] = new Regex(""" """) ^^ {x => println("scanned space");SpaceT()}
  def programStrParser: Parser[ProgramT] = new Regex("program ") ^^ {x => println("scanned program "+x.toString);ProgramT()}
  def functionParser: Parser[FunctionT]    = new Regex("function ") ^^ {x => println("scanned function "+x.toString);FunctionT()}
  def unsafeActionParser: Parser[UnsafeActionT]    = new Regex("unsafe action ") ^^ {x => println("scanned unsafe action "+x.toString);UnsafeActionT()}
  def actionParser: Parser[ActionT]    = new Regex("action ") ^^ {x => println("scanned action "+x.toString);ActionT()}
  def trueParser: Parser[TrueT]    = new Regex("true") ^^ {x => println("scanned true "+x.toString);TrueT()}
  def falseParser: Parser[FalseT]    = new Regex("false") ^^ {x => println("scanned false "+x.toString);FalseT()}
  def openParenthesisParser: Parser[OpenParenthesisT]    = new Regex("""\(""") ^^ {x => println("scanned ( "+x.toString);OpenParenthesisT()}
  def closeParenthesisParser: Parser[CloseParenthesisT]    = new Regex("""\)""") ^^ {x => println("scanned ) "+x.toString);CloseParenthesisT()}
  def openCurlyBracketParser: Parser[OpenCurlyBracketT]    = new Regex("""\{""") ^^ {x => println("scanned { "+x.toString);OpenCurlyBracketT()}
  def closeCurlyBracketParser: Parser[CloseCurlyBracketT]    = new Regex("""\}""") ^^ {x => println("scanned } "+x.toString);;CloseCurlyBracketT()}
  def doubleParser: Parser[DoubleT] = new Regex("""[-+]?[0-9]*\.?[0-9]+""") ^^ {x => println("scanned double "+x.toString);DoubleT(x.toDouble)}
  def intParser: Parser[IntegerT] = new Regex("""(0|[1-9]\d*)""") ^^ {x => println("scanned integer "+x.toString);IntegerT(x.toInt)}
  def equalParser: Parser[EqualT] = new Regex("=") ^^ {x => println("scanned = "+x.toString);EqualT()}
  def colonParser: Parser[ColonT] = new Regex(":") ^^ {x => println("scanned : "+x.toString);ColonT()}
  def commaParser: Parser[CommaT] = new Regex(",") ^^ {x => println("scanned , "+x.toString);CommaT()}
  //def emptyParser: Parser[EmptyT] = new Regex("") ^^ {x => println("scanned empty");EmptyT()}
  def newlineParser: Parser[NewlineT] = new Regex("\n") ^^ {x => println("scanned newline ");NewlineT()}
  def idParser: Parser[IdT]    = new Regex("[a-z]+") ^^ {x => println("scanned id "+x.toString);IdT(x.toString)}
  def stringParser: Parser[StringT] = new Regex("""("[^"]*")""") ^^ {x => println("scanned string "+x.toString);StringT(x.toString)}
  def typeParser: Parser[TypeT]    = new Regex("[A-Z][a-zA-Z]*") ^^ {x => println("scanned type "+x.toString);TypeT(x.toString)}
  
  
  def scan: Parser[Token] = 
  {
    (
      spaceParser               |
      programStrParser          |
      unsafeActionParser        |
      actionParser              |
      functionParser            |
      trueParser                |
      falseParser               |
      openParenthesisParser     |
      closeParenthesisParser    |
      openCurlyBracketParser    |
      closeCurlyBracketParser   |
      intParser                 |
      doubleParser              |
      equalParser               |
      colonParser               |
      commaParser               |
      //emptyParser               |
      newlineParser             |
      stringParser              |
      idParser                  |
      typeParser  
    )            
  }
  
  

}

class Token()
class DefDescriptionT() extends Token
class BasicValueT() extends Token with Callarg
class SyntaxT() extends Token

case class EmptyT() extends Token
case class TrueT() extends BasicValueT
case class FalseT() extends BasicValueT
case class ProgramT() extends DefDescriptionT
case class ActionT() extends DefDescriptionT
case class UnsafeActionT() extends DefDescriptionT
case class FunctionT() extends DefDescriptionT
case class OpenParenthesisT() extends SyntaxT
case class CloseParenthesisT() extends SyntaxT
case class OpenCurlyBracketT() extends SyntaxT
case class CloseCurlyBracketT() extends SyntaxT
case class DoubleT(val value:Double) extends BasicValueT
case class IntegerT(val value:Int) extends BasicValueT
case class EqualT() extends SyntaxT
case class ColonT() extends SyntaxT
case class CommaT() extends SyntaxT
case class NewlineT() extends SyntaxT
case class IdT(val value:String) extends Token with Callarg
case class TypeT(val value:String) extends Token
case class StringT(val value:String) extends BasicValueT
case class SpaceT() extends SyntaxT
case class DummyT() extends Token
case class EofT() extends SyntaxT







