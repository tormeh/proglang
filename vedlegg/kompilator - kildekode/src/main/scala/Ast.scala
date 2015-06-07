package fumurtCompiler

import scala.util.parsing.input.Positional


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


















class Expression() extends Positional
trait Callarg extends Positional
trait Statement extends Expression
trait BasicValueStatement extends Statement with Callarg with aCallarg with aStatement

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
case class NoArgs() extends Callarg with aCallarg

case class StringStatement(val value:String) extends BasicValueStatement
case class IntegerStatement(val value:Int) extends BasicValueStatement
case class DoubleStatement(val value:Double) extends BasicValueStatement
case class TrueStatement() extends BasicValueStatement
case class FalseStatement() extends BasicValueStatement
case class IdentifierStatement(val value:String) extends Statement with Callarg with aCallarg with aStatement
case class FunctionCallStatement(val functionidentifier:String, val args:Either[Callarg,NamedCallargs]) extends Statement with Callarg
















trait aExpression
trait aCallarg extends Callarg with aStatement
trait aStatement extends aExpression

case class aDefinition(val leftside:aDefLhs, val rightside:aDefRhs) extends aExpression
case class aDefLhs(val description:DefDescriptionT, val id:IdT, val cppid:IdT, val callingthread:String, val args:Option[aArguments], val returntype:TypeT)
case class aArguments(val args:List[aArgument])
case class aArgument(val id:IdT, cppid:IdT, val typestr:TypeT)
case class aDefRhs(val expressions:List[aExpression] )
case class aNamedCallarg(id:IdT, argument:aCallarg) //extends Callarg
case class aNamedCallargs(val value:List[aNamedCallarg])

case class aFunctionCallStatement(val functionidentifier:String, val cppfunctionidentifier:String, val args:Either[aCallarg,aNamedCallargs], val returntype:String) extends aStatement with aCallarg
