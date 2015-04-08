package fumurtCompiler

import scala.util.parsing._
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.util.parsing.input._
import scala.util.parsing.combinator._
//import scala.util.parsing.combinator.PackratParsers.PackratReader
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.PackratParsers

object FumurtParser extends Parsers //with PackratParsers
{
  override type Elem = Token
  //type Tokens = Token
  //type Token = Elem

  def parse(in:List[Token]):Either[NoSuccess, List[Definition]]=
  {
    //val ast = parseAll((progParser), in)
    val res = progParser(new TokenReader(in))
    res match
    {
      case ns:NoSuccess=>
      {
         println(res+"\n")
         //Left(new FumurtError(ns.next.pos, ns.msg, ""))
         Left(ns)
      }
      case _=>
      {
        val ast = res.get
        println("\n")
        //println(ast.toString+"\n")
        Right(ast)
      }
    }
  }
  
  
  def progParser: Parser[List[Definition]] = (paddedDefParser.+) <~ eofParser
  def paddedDefParser:Parser[Definition] = {println("paddeddefparser"); newlineParser.* ~> defParser <~ newlineParser.* }
  def defParser: Parser[Definition] = {println("defparser");  positioned((deflhsParser <~ equalParser ~ newlineParser.*) ~ defrhsParser ^^ {x=>Definition(x._1,x._2)}) }
  def deflhsParser: Parser[DefLhs] = {println("deflhsparser");  (defdescriptionParser ~ idParser ~ argsParser ~ (colonParser ~> typeParser)) ^^ {x=>DefLhs(x._1._1._1, x._1._1._2, x._1._2, x._2)} }
  def argsParser: Parser[Option[Arguments]] = {println("argsparser"); openParenthesisParser ~> ((idParser <~ colonParser) ~ typeParser ~ subsequentArgsParser.*) <~ closeParenthesisParser ^^{x=>Some(Arguments( (Argument(x._1._1, x._1._2) +: x._2).sortWith((left,right)=>left.id.value<right.id.value) ))} | emptyParser ^^ {x=>None} }
  def subsequentArgsParser: Parser[Argument] = {println("args2parserparser");  commaParser ~> (idParser <~ colonParser) ~ typeParser ^^{x=>Argument(x._1, x._2)} }
  
  def defrhsParser: Parser[DefRhs] = {println("-defrhsparser"); (openCurlyBracketParser ~ newlineParser.* ~> expressionParser ~ (newlineParser.+ ~> expressionParser).*) <~ newlineParser.* ~ closeCurlyBracketParser ^^{x=>DefRhs(x._1 +: x._2)} }
  def expressionParser: Parser[Expression] = {println("expressionparser"); positioned(defParser | statementParser) }
  /*
  def defrhsParser: Parser[DefRhs] = {println("-defrhsparser"); (openCurlyBracketParser ~> expressionParser.+) <~ newlineParser.* ~ closeCurlyBracketParser ^^{x=>DefRhs(x)} }
  def expressionParser: Parser[Expression] = {println("expressionparser"); newlineParser.+ ~> positioned(defParser | statementParser) }
  */
  def statementParser: Parser[Statement] = {println("statementparser"); functionCallParser | basicStatementParser  | identifierStatementParser }
  def callargsParser: Parser[Either[Callarg,NamedCallargs]] = {println("callargsparser"); openParenthesisParser ~> (namedcallargsParser | callargParser) <~ closeParenthesisParser ^^{x=>x match{case x:Callarg => Left(x); case x:NamedCallargs=>Right(x)}} }
  def callargParser: Parser[Callarg] = {println("callargparser"); functionCallParser | identifierStatementParser | basicStatementParser | success(NoArgs()) }
  def namedcallargsParser: Parser[NamedCallargs] = {println("namedcallargsparser"); namedcallargParser ~ subsequentnamedcallargsParser.+ ^^ {x => NamedCallargs((x._1 +: x._2).sortWith((left,right)=>left.id.value<right.id.value))} }
  def subsequentnamedcallargsParser: Parser[NamedCallarg] = {println("subsequentnamedcallargsParser"); commaParser ~> namedcallargParser }
  def namedcallargParser:Parser[NamedCallarg] = {println("namedcallargparser"); (idParser <~ equalParser) ~ callargParser ^^ {x=>NamedCallarg(x._1, x._2)} }
  def functionCallParser:Parser[FunctionCallStatement] = {println("functioncallparser"); idParser ~ callargsParser ^^ {x=>FunctionCallStatement(x._1 match{case IdT(str)=>str}, x._2)} }
  
  /*
  def argsParser: Parser[Option[Arguments]] = {println("argsparser"); openParenthesisParser ~> ((idParser <~ colonParser) ~ typeParser ~ args2Parser) <~ closeParenthesisParser ^^{x=>Some(Arguments(x._1._1, x._1._2, x._2))} | emptyParser ^^ {x=>None} }
  def args2Parser: Parser[Option[Arguments2]] = {println("args2parserparser");  commaParser ~> (idParser <~ colonParser) ~ typeParser ~ args2Parser ^^{x=>Some(Arguments2(x._1._1, x._1._2, x._2))} | emptyParser ^^^{None} }
  */
  
  
  def equalParser:Parser[Token] = accept(EqualT())
  def colonParser:Parser[Elem] = accept(ColonT())
  def commaParser:Parser[Elem] = accept(CommaT())
  def newlineParser:Parser[Elem] = accept(NewlineT())
  def emptyParser:Parser[Empty] = success(Empty())
  def openParenthesisParser:Parser[Elem] = accept(OpenParenthesisT())
  def closeParenthesisParser:Parser[Elem] = accept(CloseParenthesisT())
  def openCurlyBracketParser:Parser[Elem] = accept(OpenCurlyBracketT())
  def closeCurlyBracketParser:Parser[Elem] = accept(CloseCurlyBracketT())
  def programStrParser:Parser[Elem] = accept(ProgramT())
  def actionParser:Parser[Elem] = accept(ActionT())
  def threadParser:Parser[Elem] = accept(ThreadT())
  def functionParser:Parser[DefDescription] = accept("function", {case FunctionT() => DefDescription(FunctionT())})
  def eofParser:Parser[Elem] = accept(EofT())
  def idParser:Parser[IdT] = accept("identifier", {case IdT(value) => {println("idparser accepted "+value);IdT(value)}})
  def trueParser:Parser[Elem] = accept(TrueT())
  def falseParser:Parser[Elem] = accept(FalseT())
  def identifierStatementParser:Parser[IdentifierStatement] ={println("identifierstatementparser"); accept("identifier", {case IdT(str)=>{println("identifierstatementparser accepted "+str) ;IdentifierStatement(str)}}) }
  def basicStatementParser:Parser[BasicValueStatement] = positioned(accept("expected string, integer, boolean or float", {case StringT(value) => StringStatement(value); 
                                                                                                case IntegerT(value)=> IntegerStatement(value)
                                                                                                case DoubleT(value) => DoubleStatement(value)
                                                                                                case TrueT() => TrueStatement()
                                                                                                case FalseT() => FalseStatement()}
                                                                                                ))
  def typeParser:Parser[TypeT] = accept("expected type. Types are written with a leading capital letter", {case x:TypeT => x})
  def intParser:Parser[Elem] = accept("integer", {case x:IntegerT => x})
  def doubleParser:Parser[Elem] = accept("double", {case x:DoubleT => x})
  def defdescriptionParser: Parser[DefDescriptionT] = {println("defdescriptionParser"); accept("expected function, action, thread or program", {case x:DefDescriptionT => x}) }
  
  
  
  
  
  
  
  class TokenReader(in:List[Token]) extends Reader[Elem]
  {
    def atEnd:Boolean = in.isEmpty
    def first:Elem = in.head
    def pos:Position = in.head.pos;
    def rest = new TokenReader(in.tail)
  }
}

class AstNode()
class Expression() extends Positional
trait Callarg
trait Statement extends Expression
trait BasicValueStatement extends Statement with Callarg

case class Definition(val leftside:DefLhs, val rightside:DefRhs) extends Expression
case class DefLhs(val description:DefDescriptionT, val id:IdT, val args:Option[Arguments], val returntype:TypeT)
/*case class Arguments(val id:IdT, val typestr:TypeT, val args2:Option[Arguments2])
case class Arguments2(val id:IdT, val typestr:TypeT, val args2:Option[Arguments2])*/
case class Arguments(val args:List[Argument])
case class Argument(val id:IdT, val typestr:TypeT)
case class DefRhs(val expressions:List[Expression] )
case class Empty();
case class DefDescription(val value:Token)
case class NamedCallarg(id:IdT, argument:Callarg) //extends Callarg
case class NamedCallargs(val value:List[NamedCallarg])
case class NoArgs() extends Callarg

case class StringStatement(val value:String) extends BasicValueStatement
case class IntegerStatement(val value:Int) extends BasicValueStatement
case class DoubleStatement(val value:Double) extends BasicValueStatement
case class TrueStatement() extends BasicValueStatement
case class FalseStatement() extends BasicValueStatement
case class IdentifierStatement(val value:String) extends Statement with Callarg
case class FunctionCallStatement(val functionidentifier:String, val args:Either[Callarg,NamedCallargs]) extends Statement with Callarg






