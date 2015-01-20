package fumurtCompiler

import scala.language.implicitConversions
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex

object FumurtScanner2 extends RegexParsers //with Parsers
{
  def scan(in:String, opts:Options):Either[FumurtError, List[Token]] =
  {
   println(in)
  
   println(parseAll(scanTokens, in))
  
   Left(new FumurtError(Global, "scanscan"))
  }
 
  private def scanTokens: Parser[List[Token]] = (scanComment*) ~ (scanToken*) ^^ {case _~ l => l:+EofT()}

  private def scanToken: Parser[Token] = (scanComment*) ~ (
      scanParenthesisOpen   |
      scanParenthesisClose  |
      scanCurlyBracketsOpen   |
      scanCurlyBracketsClose  |
      scanComma  |
      scanColon  |
      scanDefAs  |
      scanProgram   |
      scanFunction    |
      scanAction    |
      scanUnsafeAction    |
      //
      /*scanIf     |
      scanThen   |
      scanElse   |
      scanFi     |*/
      //
      //scanNum    |
      scanTrue   |
      scanFalse  |
      //
      scanTBool  |
      scanTNat   |
      scanVar    ) ^^ {case _~t => t}
 
  //def word:Parser[String] = new Regex("[a-z]+") ^^ {_.toString}
 
  private def scanComment = commentRegex
  private def scanParenthesisOpen = positioned(OpenParenthesisLex.r ^^ { _ => OpenT() })
  private def scanParenthesisClose = positioned(CloseParenthesisLex.r ^^ { _ => CloseT() })
  private def scanCurlyBracketsOpen = positioned(OpenCurlyBracketLex.r ^^ { _ => OpenT() })
  private def scanCurlyBracketsClose = positioned(CloseCurlyBracketLex.r ^^ { _ => CloseT() })
  private def scanComma = positioned(CommaLex.r ^^ { _ => CommaT() })
  private def scanColon = positioned(ColonLex.r ^^ { _ => ColonT() })
  private def scanDefAs = positioned(DefAsLex.r ^^ { _ => DefAsT() })
  private def scanProgram = positioned(ProgramLex.r ^^ { _ => MainT() })
  private def scanFunction = positioned(FunctionLex.r ^^ { _ => DefT() })
  private def scanAction = positioned(ActionLex.r ^^ { _ => DefT() })
  private def scanUnsafeAction = positioned(UnsafeActionLex.r ^^ { _ => DefT() })

  /*private def scanIf = positioned(IfLex.r ^^ { _ => IfT() })
  private def scanThen = positioned(ThenLex.r ^^ { _ => ThenT() })
  private def scanElse = positioned(ElseLex.r ^^ { _ => ElseT() })
  private def scanFi = positioned(FiLex.r ^^ { _ => FiT() })
  */

  // TODO: better error msg: regex `\z` expected means: identifiers must start with small letter
  private def scanVar:Parser[Token] = positioned(varRegex ^^ { n => VarT(n) })
  
  private def scanNum = positioned(numRegex ^^ { n => NumT(n.toInt) })
  private def scanTrue = positioned(TrueLex.r ^^ { _ => TrueT() })
  private def scanFalse = positioned(FalseLex.r ^^ { _ => FalseT() })
  
  private def scanTBool = positioned(TBoolLex.r ^^ { _ => BoolT() })
  private def scanTNat = positioned(TNatLex.r ^^ { _ => NatT() })

  private val commentRegex: Regex = """--[^\n]*\n""".r
  private val numRegex: Regex = """\d+""".r
  private val varRegex: Regex = """[a-zA-Z][a-zA-Z0-9]*""".r

  private val OpenParenthesisLex  = """\("""
  private val CloseParenthesisLex = """\)"""
  private val OpenCurlyBracketLex  = """\{"""
  private val CloseCurlyBracketLex = """\}"""
  private val CommaLex = ""","""
  private val ColonLex = """\:"""
  private val DefAsLex = """="""
  private val ProgramLex  = """program"""
  private val FunctionLex   = """function"""
  private val ActionLex   = """action"""
  private val UnsafeActionLex = """unsafe action"""
  /*private val IfLex    = """IF"""
  private val ThenLex  = """THEN"""
  private val ElseLex  = """ELSE"""
  private val FiLex    = """FI"""*/
  private val TrueLex  = """true"""
  private val FalseLex = """false"""
  private val TBoolLex = """Boolean"""
  private val TNatLex  = """nat"""
}
