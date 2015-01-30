package fumurtCompiler

import scala.util.parsing._
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.util.parsing.input._
import scala.util.parsing.combinator._
//import scala.util.parsing.combinator.PackratParsers.PackratReader
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.PackratParsers

object FumurtParser extends TokenParsers //with PackratParsers
{
  type Elem = Token
  type Tokens = Token
  //type Token = Elem

  def parse(in:List[Token]):Either[FumurtError, List[Definition]]=
  {
    //val ast = parseAll((progParser), in)
    val ast = progParser(new TokenReader(in))
    println(ast.toString)
    Left(FumurtError(Global, "testerror"))
  }
  
  
  def progParser: Parser[List[Definition]] = (defParser.*) <~ eofParser
  //def progParser: Parser[Definition] = defParser ~ (eofParser | (newlinesParser ~ progParser))
  def defParser: Parser[Definition] = (deflhsParser <~ equalParser ~ optionalNewlinesParser) ~ defrhsParser ^^ {x=>Definition(x._1,x._2)}
  def deflhsParser: Parser[DefLhs] = (defdescriptionParser ~ idParser ~ argsParser) ^^ {x=>DefLhs(x._1._1, x._1._2, x._2)}
  def argsParser: Parser[MaybeArguments] = openParenthesisParser ~> ((idParser <~ colonParser) ~ typeParser ~ args2Parser) <~ closeParenthesisParser | emptyParser ^^ {x=>MaybeArguments(None)}
  def args2Parser: Parser[MaybeArguments2] = commaParser ~> (idParser <~ colonParser) ~ typeParser ~ args2Parser | emptyParser
  def defrhsParser: Parser[DefRhs] = openCurlyBracketParser ~ expressionParser ~ closeCurlyBracketParser
  def expressionParser: Parser[Expression] = defParser ~ newlineParser ~ expressionParser | statementParser ~ newlineParser ~ expressionParser | (emptyParser ^^^ Statement())
  def statementParser: Parser[Statement] = basicStatementParser | idParser ~ callargsParser
  def callargsParser: Parser[Callarg] = openParenthesisParser ~> (idParser | basicStatementParser | callargs2Parser) <~ closeParenthesisParser
  def callargs2Parser: Parser[Callargs2] = idParser <~ equalParser ~> idParser <~ commaParser ~> idParser <~ equalParser ~> idParser ~ callargs3Parser
  def callargs3Parser: Parser[Callargs3] = commaParser ~> idParser <~ equalParser ~> idParser | commaParser ~> idParser <~ equalParser ~> idParser ~ callargs3Parser | emptyParser
  
  
  def equalParser:Parser[Token] = accept(EqualT())
  def colonParser:Parser[Elem] = accept(EmptyT())
  def commaParser:Parser[Elem] = accept(CommaT())
  def newlineParser:Parser[Elem] = accept(NewlineT())
  def newlinesParser:Parser[List[Elem]] = newlineParser ~> newlineParser.*
  def optionalNewlinesParser:Parser[List[Elem]] = newlineParser.*
  def emptyParser:Parser[Empty] = success(Empty())
  def openParenthesisParser:Parser[Elem] = accept(OpenParenthesisT())
  def closeParenthesisParser:Parser[Elem] = accept(CloseParenthesisT())
  def openCurlyBracketParser:Parser[Elem] = accept(OpenCurlyBracketT())
  def closeCurlyBracketParser:Parser[Elem] = accept(CloseCurlyBracketT())
  def programStrParser:Parser[Elem] = accept(ProgramT())
  def actionParser:Parser[Elem] = accept(ActionT())
  def unsafeactionParser:Parser[Elem] = accept(UnsafeActionT())
  def functionParser:Parser[DefDescription] = accept("function", {case FunctionT() => DefDescription(FunctionT())})
  def eofParser:Parser[Elem] = accept(EofT())
  def idParser:Parser[IdT] = accept("identifier", {case IdT(value) => IdT(value)})
  def trueParser:Parser[Elem] = accept(TrueT())
  def falseParser:Parser[Elem] = accept(FalseT())
  def basicStatementParser:Parser[BasicValueT] = accept("expected string, integer, boolean or float", {case StringT(value) => StringT(value); 
                                                                                                case IntegerT(value)=> IntegerT(value)
                                                                                                case DoubleT(value) => DoubleT(value)
                                                                                                case TrueT() => TrueT()
                                                                                                case FalseT() => FalseT()})
  def typeParser:Parser[Elem] = accept("expected type. Types are written with a leading capital letter", {case TypeT(value) => TypeT(value)})
  def intParser:Parser[Elem] = accept("integer", {case IntegerT(value) => IntegerT(value)})
  def doubleParser:Parser[Elem] = accept("double", {case DoubleT(value) => DoubleT(value)})
  def defdescriptionParser: Parser[DefDescriptionT] = accept("expected function, action, unsafe action or program", {case FunctionT() => FunctionT()
                                                                                                case ActionT() => ActionT()
                                                                                                case UnsafeActionT() => UnsafeActionT()
                                                                                                case ProgramT() => ProgramT()})
  
  
  
  
  
  
  


  class TokenReader(in:List[Token]) extends Reader[Elem]
  {
    def atEnd:Boolean = in.isEmpty
    def first:Elem = in.head
    def pos:Position = Global;
    def rest = new TokenReader(in.tail)
  }
}

class AstNode()
class Expression()
trait Callarg

case class Definition(val leftside:DefLhs, val rightside:DefRhs) extends Expression
case class DefLhs(val description:DefDescriptionT, val id:IdT, val args:MaybeArguments)
case class Arguments(val id:String, val typestr:String, val args2:Option[Arguments])
case class DefRhs(val expression:Expression )
case class Statement() extends Expression
case class Empty();
case class DefDescription(val value:Token)
case class MaybeArguments(val value:Option[Arguments])
case class MaybeArguments2(val value:Option[Arguments2])
case class Arguments2(val id:String, val typestr:String, val args2:MaybeArguments)
case class Callargs2(id:Option[IdT], basicStatement:Option[BasicValueT], callargs2:Option[Callargs2]) extends Callarg








