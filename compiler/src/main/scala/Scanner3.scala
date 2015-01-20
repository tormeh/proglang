//https://wiki.scala-lang.org/display/SW/Parser+Combinators--Getting+Started
//http://en.wikipedia.org/wiki/Regular_expression#Syntax


package fumurtCompiler

import scala.language.implicitConversions
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex

object FumurtScanner extends RegexParsers with Parsers
{
  def scan(in:String, opts:Options):Either[FumurtError, List[Token]] =
  {
   println(in)
  
   println(parseAll(scan, in))
  
   Left(new FumurtError(Global, "scanscan"))
  }

  def programStrParser: Parser[ProgramT] = new Regex("program") ^^ {_ => ProgramT()}
  def functionParser: Parser[FunctionT]    = new Regex("function") ^^ {_ => FunctionT()}
  def unsafeActionParser: Parser[UnsafeActionT]    = new Regex("unsafe action") ^^ {_ => UnsafeActionT()}
  def actionParser: Parser[ActionT]    = new Regex("action") ^^ {_ => ActionT()}
  def trueParser: Parser[TrueT]    = new Regex("true") ^^ {_ => TrueT()}
  def falseParser: Parser[FalseT]    = new Regex("false") ^^ {_ => FalseT()}
  def openParenthesisParser: Parser[OpenParenthesisT]    = new Regex("""\(""") ^^ {_ => OpenParenthesisT()}
  def closeParenthesisParser: Parser[CloseParenthesisT]    = new Regex("""\)""") ^^ {_ => CloseParenthesisT()}
  def openCurlyBracketParser: Parser[OpenCurlyBracketT]    = new Regex("""\{""") ^^ {_ => OpenCurlyBracketT()}
  def closeCurlyBracketParser: Parser[CloseCurlyBracketT]    = new Regex("""\}""") ^^ {_ => CloseCurlyBracketT()}
  def doubleParser: Parser[DoubleT] = new Regex("""[-+]?[0-9]*\.?[0-9]+""") ^^ {_ => DoubleT(_.toDouble)}
  def intParser: Parser[IntegerT] = new Regex("""(0|[1-9]\d*)""") ^^ {_ => IntegerT(_.toInt)}
  def equalParser: Parser[EqualT] = new Regex("=") ^^ {_ => EqualT()}
  def colonParser: Parser[ColonT] = new Regex(":") ^^ {_ => ColonT()}
  def commaParser: Parser[CommaT] = new Regex(",") ^^ {_ => CommaT()}
  def emptyParser: Parser[EmptyT] = new Regex("") ^^ {_ => EmptyT()}
  def newlineParser: Parser[NewlineT] = new Regex("\n") ^^ {_ => NewlineT()}
  def idParser: Parser[idT]    = new Regex("[a-z]+") ^^ {_ => idT(_.toString)}
  def typeParser: Parser[TypeT]    = new Regex("[A-Z]+") ^^ {_ => TypeT(_.toString)}
  
  def scan: Parser[List[Token]] = 
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
    emptyParser               |
    newlineParser             |
    idParser                  |
    typeParser                |
  
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
case class OpenCurlyBracket() extends Token
case class CloseCurlyBracketT() extends Token







