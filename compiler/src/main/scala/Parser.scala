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
  def defParser: Parser[Definition] = (deflhsParser ~ equalParser ~ defrhsParser) ^^ {(x:DefLhs,y:DefRhs)=>Definition(x,y)}
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
  
  
  def equalParser:Parser[Token] = accept(EqualT())
  def colonParser:Parser[Elem] = accept(EmptyT())
  def commaParser:Parser[Elem] = accept(CommaT())
  def newlineParser:Parser[Elem] = accept(NewlineT())
  def newlinesParser:Parser[List[Elem]] = newlineParser ~> newlineParser.*
  def emptyParser:Parser[Elem] = success(EmptyT())
  def openParenthesisParser:Parser[Elem] = accept(OpenParenthesisT())
  def closeParenthesisParser:Parser[Elem] = accept(CloseParenthesisT())
  def openCurlyBracketParser:Parser[Elem] = accept(OpenCurlyBracketT())
  def closeCurlyBracketParser:Parser[Elem] = accept(CloseCurlyBracketT())
  def programStrParser:Parser[Elem] = accept(ProgramT())
  def actionParser:Parser[Elem] = accept(ActionT())
  def unsafeactionParser:Parser[Elem] = accept(UnsafeActionT())
  def functionParser:Parser[Elem] = accept(FunctionT())
  def eofParser:Parser[Elem] = accept(EofT())
  def idParser:Parser[Elem] = accept("identifier", {case IdT(value) => IdT(value)})
  def trueParser:Parser[Elem] = accept(TrueT())
  def falseParser:Parser[Elem] = accept(FalseT())
  
  /*def equalParser:Parser[Token] = accept("colon":String, EqualT)
  def colonParser:Parser[Elem] = accept("colon":String, {case ColonT() => EmptyT()})
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
  def eofParser:Parser[Elem] = accept("end of file":String, {case EofT() => })*/


  class TokenReader(in:List[Token]) extends Reader[Elem]
  {
    def atEnd:Boolean = in.isEmpty
    def first:Elem = in.head
    def pos:Position = Global
    def rest = new TokenReader(in.tail)
  }
}

class AstNode()
class Expression()

case class Definition(val leftside:DefLhs, val rightside:DefRhs) extends Expression
case class DefLhs(val description:String, val id:String, val args:PArguments)
case class PArguments(val id:String, val typestr:String, val args2:Either[Empty, PArguments])
case class DefRhs(val expression:Expression )
case class Statement() extends Expression
case class Empty();





//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*



class PackratReader[+T](underlying: Reader[T]) extends Reader[T] { outer =>

    /*
     * caching of intermediate parse results and information about recursion
     */
    private[PackratParsers] val cache = mutable.HashMap.empty[(Parser[_], Position), MemoEntry[_]]

    private[PackratParsers] def getFromCache[T](p: Parser[T]): Option[MemoEntry[T]] = {
      cache.get((p, pos)).asInstanceOf[Option[MemoEntry[T]]]
    }

    private[PackratParsers] def updateCacheAndGet[T](p: Parser[T], w: MemoEntry[T]): MemoEntry[T] = {
      cache.put((p, pos),w)
      w
    }

    /* a cache for storing parser heads: allows to know which parser is involved
       in a recursion*/
    private[PackratParsers] val recursionHeads: mutable.HashMap[Position, Head] = mutable.HashMap.empty

    //a stack that keeps a list of all involved rules
    private[PackratParsers] var lrStack: List[LR] = Nil

    override def source: java.lang.CharSequence = underlying.source
    override def offset: Int = underlying.offset

    def first: T = underlying.first
    def rest: Reader[T] = new PackratReader(underlying.rest) {
      override private[PackratParsers] val cache = outer.cache
      override private[PackratParsers] val recursionHeads = outer.recursionHeads
      lrStack = outer.lrStack
    }

    def pos: Position = underlying.pos
    def atEnd: Boolean = underlying.atEnd
  }
  
*/
