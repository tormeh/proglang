//https://wiki.scala-lang.org/display/SW/Parser+Combinators--Getting+Started
//http://en.wikipedia.org/wiki/Regular_expression#Syntax


package fumurtCompiler

import scala.language.implicitConversions
import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.Positional

object FumurtScanner extends RegexParsers /*with Parsers*/
{
  override val skipWhitespace = false
  
  def scan(in:String, opts:Options):Either[NoSuccess, List[Token]] =
  {
    println(in) 
   
    parseAll((scan*), in) match
    {
      case Success(result, _) =>
      {
        val tokens = result.filter(x=>x match{case SpaceT() => false; case _ => true}) :+ EofT()
        Right(tokens)
      }
      case f:Failure => Left(f)
      case e:Error => Left(e)
      //case Failure(message, reader) => Left(new FumurtError(reader.pos, "Failure: "+message,"\n" + in.lines.toList(reader.pos.line) +"\n"))
      //case Error(message,_) => Left(new FumurtError(Global, "Error: " + message, ""))
    }
   
  }
  
  
  def removeSpaces(intokens:List[Token]):List[Token] =
  {
    intokens
  }

  def spaceParser:Parser[SpaceT] = positioned( new Regex(""" """) ^^ {x => println("scanned space");SpaceT()} )
  def programStrParser: Parser[ProgramT] = positioned( new Regex("program ") ^^ {x => println("scanned program "+x.toString);ProgramT()} )
  def functionParser: Parser[FunctionT] = positioned( new Regex("function ") ^^ {x => println("scanned function "+x.toString);FunctionT()} )
  def threadParser: Parser[ThreadT] = positioned( new Regex("thread ") ^^ {x => println("scanned thread "+x.toString);ThreadT()} )
  def synchronizedVariableParser: Parser[SynchronizedVariableT] = positioned(new Regex("synchronized variable ") ^^ {x => println("scanned synchronized variable "+x.toString); SynchronizedVariableT()})
  def valueParser: Parser[ValueT] = positioned( new Regex("value ") ^^ {x => println("scanned unsafe value "+x.toString);ValueT()} )
  def actionParser: Parser[ActionT] = positioned( new Regex("action ") ^^ {x => println("scanned action "+x.toString);ActionT()} )
  def trueParser: Parser[TrueT] = positioned( new Regex("true") ^^ {x => println("scanned true "+x.toString);TrueT()} )
  def falseParser: Parser[FalseT] = positioned( new Regex("false") ^^ {x => println("scanned false "+x.toString);FalseT()} )
  def openParenthesisParser: Parser[OpenParenthesisT] = positioned( new Regex("""\(""") ^^ {x => println("scanned ( "+x.toString);OpenParenthesisT()} )
  def closeParenthesisParser: Parser[CloseParenthesisT] = positioned( new Regex("""\)""") ^^ {x => println("scanned ) "+x.toString);CloseParenthesisT()} )
  def openCurlyBracketParser: Parser[OpenCurlyBracketT] = positioned( new Regex("""\{""") ^^ {x => println("scanned { "+x.toString);OpenCurlyBracketT()} )
  def closeCurlyBracketParser: Parser[CloseCurlyBracketT] = positioned( new Regex("""\}""") ^^ {x => println("scanned } "+x.toString);;CloseCurlyBracketT()} )
  def doubleParser: Parser[DoubleT] = positioned( new Regex("""[-+]?[0-9]*\.?[0-9]+""") ^^ {x => println("scanned double "+x.toString);DoubleT(x.toDouble)} )
  def intParser: Parser[IntegerT] = positioned( new Regex("""(0|[1-9]\d*)""") ^^ {x => println("scanned integer "+x.toString);IntegerT(x.toInt)} )
  def equalParser: Parser[EqualT] = positioned( new Regex("=") ^^ {x => println("scanned = "+x.toString);EqualT()} )
  def colonParser: Parser[ColonT] = positioned( new Regex(":") ^^ {x => println("scanned : "+x.toString);ColonT()} )
  def commaParser: Parser[CommaT] = positioned( new Regex(",") ^^ {x => println("scanned , "+x.toString);CommaT()} )
  //def emptyParser: Parser[EmptyT] = new Regex("") ^^ {x => println("scanned empty");EmptyT()}
  def newlineParser: Parser[NewlineT] = positioned( new Regex("\n") ^^ {x => println("scanned newline ");NewlineT()} )
  def idParser: Parser[IdT] = positioned( new Regex("[a-z]+[a-zA-Z]*") ^^ {x => println("scanned id "+x.toString);IdT(x.toString)} )
  def stringParser: Parser[StringT] = positioned( new Regex("""("[^"]*")""") ^^ {x => println("scanned string "+x.toString);StringT(x.toString)} )
  def typeParser: Parser[TypeT] = positioned( new Regex("[A-Z][a-zA-Z]*") ^^ {x => println("scanned type "+x.toString);TypeT(x.toString)} )
  
  
  def scan: Parser[Token] = 
  {
    (
      spaceParser               |
      programStrParser          |
      threadParser        |
      actionParser              |
      synchronizedVariableParser  |
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

abstract class Token() extends Positional
abstract class DefDescriptionT() extends Token
abstract class BasicValueT() extends Token
abstract class SyntaxT() extends Token

case class EmptyT() extends Token
case class TrueT() extends BasicValueT {override def toString = "true"}
case class FalseT() extends BasicValueT {override def toString = "false"}
case class ProgramT() extends DefDescriptionT {override def toString = "program"}
case class ActionT() extends DefDescriptionT {override def toString = "action"}
case class ThreadT() extends DefDescriptionT {override def toString = "thread"}
case class FunctionT() extends DefDescriptionT {override def toString = "function"}
case class ValueT() extends DefDescriptionT {override def toString = "value"}
case class SynchronizedVariableT() extends DefDescriptionT {override def toString = "synchronized variable"}
case class OpenParenthesisT() extends SyntaxT {override def toString = "\"(\""}
case class CloseParenthesisT() extends SyntaxT {override def toString = "\")\""}
case class OpenCurlyBracketT() extends SyntaxT {override def toString = "\"{\""}
case class CloseCurlyBracketT() extends SyntaxT {override def toString = "\"}\""}
case class DoubleT(val value:Double) extends BasicValueT {override def toString = "decimal number"}
case class IntegerT(val value:Int) extends BasicValueT {override def toString = "integer"}
case class EqualT() extends SyntaxT {override def toString = "\"=\""}
case class ColonT() extends SyntaxT {override def toString = "\":\""}
case class CommaT() extends SyntaxT {override def toString = "\",\""}
case class NewlineT() extends SyntaxT {override def toString = "newline"}
case class IdT(val value:String) extends Token {override def toString = "identifier(\""+value+"\")"}
case class TypeT(val value:String) extends Token {override def toString = "type(\""+value+"\")"}
case class StringT(val value:String) extends BasicValueT {override def toString = "string"}
case class SpaceT() extends SyntaxT
case class DummyT() extends Token
case class EofT() extends SyntaxT {override def toString = "end of file"}








