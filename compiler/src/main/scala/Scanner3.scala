//https://wiki.scala-lang.org/display/SW/Parser+Combinators--Getting+Started
//http://en.wikipedia.org/wiki/Regular_expression#Syntax


package fumurtCompiler

import scala.language.implicitConversions
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex

object FumurtScanner extends RegexParsers /*with Parsers*/
{
  def scan(in:String, opts:Options):Either[FumurtError, List[Token]] =
  {
   println(in)
  
   println(parseAll((scan*), in))
  
   Left(new FumurtError(Global, "testerror"))
  }

  def programStrParser: Parser[ProgramT] = new Regex("program") ^^ {x => println("scanned program "+x.toString);ProgramT()}
  def functionParser: Parser[FunctionT]    = new Regex("function") ^^ {x => println("scanned function "+x.toString);FunctionT()}
  def unsafeActionParser: Parser[UnsafeActionT]    = new Regex("unsafe action") ^^ {x => println("scanned unsafe action "+x.toString);UnsafeActionT()}
  def actionParser: Parser[ActionT]    = new Regex("action") ^^ {x => println("scanned action "+x.toString);ActionT()}
  def trueParser: Parser[TrueT]    = new Regex("true") ^^ {x => println("scanned true "+x.toString);TrueT()}
  def falseParser: Parser[FalseT]    = new Regex("false") ^^ {x => println("scanned false "+x.toString);FalseT()}
  def openParenthesisParser: Parser[OpenParenthesisT]    = new Regex("""\(""") ^^ {x => println("scanned ( "+x.toString);OpenParenthesisT()}
  def closeParenthesisParser: Parser[CloseParenthesisT]    = new Regex("""\)""") ^^ {x => println("scanned ) "+x.toString);CloseParenthesisT()}
  def openCurlyBracketParser: Parser[OpenCurlyBracketT]    = new Regex("""\{""") ^^ {x => println("scanned { "+x.toString);OpenCurlyBracketT()}
  def closeCurlyBracketParser: Parser[CloseCurlyBracketT]    = new Regex("""\}""") ^^ {x => println("scanned } "+x.toString);;CloseCurlyBracketT()}
  def doubleParser: Parser[DoubleT] = new Regex("""[-+]?[0-9]*\.?[0-9]+""") ^^ {x => println("scanned "+x.toString);DoubleT(x.toDouble)}
  def intParser: Parser[IntegerT] = new Regex("""(0|[1-9]\d*)""") ^^ {x => println("scanned "+x.toString);IntegerT(x.toInt)}
  def equalParser: Parser[EqualT] = new Regex("=") ^^ {x => println("scanned = "+x.toString);EqualT()}
  def colonParser: Parser[ColonT] = new Regex(":") ^^ {x => println("scanned : "+x.toString);ColonT()}
  def commaParser: Parser[CommaT] = new Regex(",") ^^ {x => println("scanned , "+x.toString);CommaT()}
  //def emptyParser: Parser[EmptyT] = new Regex("") ^^ {x => println("scanned empty");EmptyT()}
  def newlineParser: Parser[NewlineT] = new Regex("\n") ^^ {x => println("scanned newline "+x.toString);NewlineT()}
  def idParser: Parser[IdT]    = new Regex("[a-z]+") ^^ {x => println("scanned id "+x.toString);IdT(x.toString)}
  def typeParser: Parser[TypeT]    = new Regex("[A-Z][a-zA-Z]*") ^^ {x => println("scanned type "+x.toString);TypeT(x.toString)}
  
  
  def scan: Parser[Token] = 
  {
    (
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
      doubleParser              |
      intParser                 |
      equalParser               |
      colonParser               |
      commaParser               |
      //emptyParser               |
      newlineParser             |
      idParser                  |
      typeParser  
    )            
  }
  
  /*def progParser: Parser[Program] = defParser ~ (eofParser | (newlineParser ~ progParser))
  def DefParser: Parser[Definition] = deflhsParser ~ equalParser ~ defrhsParser
  def deflhsParser: Parser[DefLhs] = Defdescription ~ idParser ~ argsParser
  def argsParser: Parser[Arguments] = (openParenthesisParser ~ idParser ~ colonParser ~ typeParser ~ args2Parser ~ closeParenthesisParser) | 
  def args2Parser: Parser[Arguments2] = commaParser ~ idParser ~ colonParser ~ typeParser ~ args2Parser | emptyParser
  def defdescriptionParser: Parser[DefDescription] = programStrParser | unsafeactionParser | actionParser | functionParser
  def defrhsParser: Parser[DefRhs] = openCurlyBracketParser ~ expression ~ closeCurlyBracketParser
  def expressionParser: parser[Expression] = defParser ~ newlineParser ~ expressionParser | statementParser ~ newlineParser ~ expressionParser | emptyParser
  def statementParser: Parser[Statement] = idParser ~ callargsParser | basic
  def callargsParser: Parser[Callargs] = openParenthesisParser ~ (idParser | intParser | doubleParser | trueParser | falseParser | callargs2Parser) ~ closeParenthesisParser 
  def callargs2Parser: Parser[Callargs2] = idParser ~ equalParser ~ idParser ~ commaParser ~ idParser ~ equalParser ~ idParser ~ callargs3Parser
  def callargs3Parser: parser[Callargs3] = commaParser ~ idParser ~ equalParser ~ idParser | commaParser ~ idParser ~ equalParser ~ idParser ~ callargs3Parser | emptyParser*/
  

}

class Token()

case class EmptyT() extends Token
case class TrueT() extends Token
case class FalseT() extends Token
case class ProgramT() extends Token
case class ActionT() extends Token
case class UnsafeActionT() extends Token
case class FunctionT() extends Token
case class OpenParenthesisT() extends Token
case class CloseParenthesisT() extends Token
case class OpenCurlyBracketT() extends Token
case class CloseCurlyBracketT() extends Token
case class DoubleT(val value:Double) extends Token
case class IntegerT(val value:Int) extends Token
case class EqualT() extends Token
case class ColonT() extends Token
case class CommaT() extends Token
case class NewlineT() extends Token
case class IdT(val value:String) extends Token
case class TypeT(val value:String) extends Token








