//https://wiki.scala-lang.org/display/SW/Parser+Combinators--Getting+Started
//http://en.wikipedia.org/wiki/Regular_expression#Syntax


package fumurtCompiler

import scala.language.implicitConversions
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex

object FumurtScanner extends RegexParsers //with Parsers
{
  def scan(in:String, opts:Options):Either[FumurtError, List[Token]] =
  {
   println(in)
  
   println(parseAll(scanTokens, in))
  
   Left(new FumurtError(Global, "scanscan"))
  }
 
  
}

object SimpleParser extends RegexParsers {

  def wordParser: Parser[String]    = new Regex("[a-z]+") ^^ { _.toString }
  def functionParser: Parser[String]    = new Regex("function") ^^ { _.toString }
  def actionParser: Parser[String]    = new Regex("action") ^^ { _.toString }
  def unsafeActionParser: Parser[String]    = new Regex("unsafe action") ^^ { _.toString }
  def trueParser: Parser[String]    = new Regex("true") ^^ { _.toString }
  def falseParser: Parser[String]    = new Regex("false") ^^ { _.toString }
  def openParenthsisParser: Parser[String]    = new Regex("\(") ^^ { _.toString }
  def closeParenthsisParser: Parser[String]    = new Regex("\)") ^^ { _.toString }
  def openCurlyBracketParser: Parser[String]    = new Regex("\{") ^^ { _.toString }
  def closeCurlyBracketParser: Parser[String]    = new Regex("\}") ^^ { _.toString }
  def floatParser: Parser[Float] = floatingPointNumber ^^ (_.toFloat)
  def intParser: Parser[Int] = new Regex("(0|[1-9]\d*)") ^^ (_.toInt)

}
