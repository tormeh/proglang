package fumurtCompiler

import scala.util.parsing.combinator._
import scala.language.postfixOps
import scala.language.implicitConversions

object FumurtParser extends Parsers
{
  type Elem = fumurtCompiler.FumurtScanner.Token
  type Token = Elem

  def parse(in:List[Token]):Either[FumurtError, List[Definition]]=
  {
    val ast = parseAll(progParser*, in)
    println(ast.toString)
    Left(FumurtError(Global, "testerror"))
  }
  
  
  def progParser: Parser[Definition] = defParser ~ (eofParser | (newlineParser ~> progParser))
  def DefParser: Parser[Definition] = deflhsParser <~ equalParser ~> defrhsParser ^^ {(x,y)=>Definition(x,y)}
  def deflhsParser: Parser[DefLhs] = defdescriptionParser ~ idParser ~ argsParser
  def argsParser: Parser[Either[Empty,Arguments]] = (openParenthesisParser ~ idParser ~ colonParser ~ typeParser ~ args2Parser ~ closeParenthesisParser) | emptyParser
  def args2Parser: Parser[Arguments2] = commaParser ~ idParser ~ colonParser ~ typeParser ~ args2Parser | emptyParser
  def defdescriptionParser: Parser[DefDescription] = programStrParser | unsafeactionParser | actionParser | functionParser
  def defrhsParser: Parser[DefRhs] = openCurlyBracketParser ~ expression ~ closeCurlyBracketParser
  def expressionParser: parser[Expression] = defParser ~ newlineParser ~ expressionParser | statementParser ~ newlineParser ~ expressionParser | emptyParser
  def statementParser: Parser[Statement] = idParser ~ callargsParser | basic
  def callargsParser: Parser[Callargs] = openParenthesisParser ~> (idParser | intParser | doubleParser | trueParser | falseParser | callargs2Parser) <~ closeParenthesisParser 
  def callargs2Parser: Parser[Callargs2] = idParser <~ equalParser ~> idParser <~ commaParser ~> idParser <~ equalParser ~> idParser ~ callargs3Parser
  def callargs3Parser: Parser[Callargs3] = commaParser ~> idParser <~ equalParser ~> idParser | commaParser ~> idParser <~ equalParser ~> idParser ~ callargs3Parser | emptyParser
  
  
  def equalParser:Parser[Token] = accept("colon":String, {case EqualT() => })
  def colonParser:Parser[Elem] = accept("colon":String, {case ColonT() => })
  def commaParser:Parser[Elem] = accept("colon":String, {case CommaT() => })
  def newlineParser:Parser[Elem] = accept("colon":String, {case NewlineT() => })
  def emptyParser:Parser[Elem] = success()
  def openParenthesisParser:Parser[Elem] = accept("colon":String, {case OpenParenthesisT() => })
  def closeParenthesisParser:Parser[Elem] = accept("colon":String, {case CloseParenthesisT() => })
  def openCurlyBracketParser:Parser[Elem] = accept("colon":String, {case OpenCurlyBracketT() => })
  def closeCurlyBracketParser:Parser[Elem] = accept("colon":String, {case CloseCurlyBracketT() => })
  def programStrParser:Parser[Elem] = accept("colon":String, {case ProgramT() => })
  def actionParser:Parser[Elem] = accept("colon":String, {case ActionT() => })
  def unsafeactionParser:Parser[Elem] = accept("colon":String, {case UnsafeActionT() => })
  def functionParser:Parser[Elem] = accept("colon":String, {case FunctionT() => })

}

class AstNode()

case class Definition(val leftside:DefLhs, val rightside:DefRhs)
case class DefLhs(val description:String, val id:String, val args:PArguments)
case class PArguments(val id:String, val typestr:String, val args2:Either[Empty, PArguments])
case class DefRhs(val expression:Expression )
case class Expression()
case class Empty();
