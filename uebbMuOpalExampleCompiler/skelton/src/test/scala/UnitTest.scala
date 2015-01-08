  package de.tuberlin.uebb.comp1.homework
  import org.specs2.mutable._
  import scala.io.Source._

  class UnitTest extends Specification {
	testAdd()
	testSub()
	testMul()
	testDiv()
	"IF eq(2,3) THEN false ELSE eq(2,2) FI" should {
		  "be equal true" in {
		    interpretTest("eq") must beRight(BoolV(true))
		  }
	}  
	"IF lt(3,4) THEN 5 FI" should {
		  "be equal 5" in {
		    interpretTest("lt") must beRight(NumV(5))
		  }
	}
	"IF or(true, false) THEN\n\tIF or(true, true) THEN or(false, false) ELSE true FI\nFI" should {
		  "be equal false" in {
		    interpretTest("or") must beRight(BoolV(false))
		  }
	}
	"IF and(true, true) THEN\n\tIF and(false, false) THEN true ELSE and(true, false) FI\nFI" should {
		  "be equal false" in {
		    interpretTest("and") must beRight(BoolV(false))
		  }
	}
	"IF not(false) THEN not(true) FI" should {
		  "be equal false" in {
		    interpretTest("not") must beRight(BoolV(false))
		  }
	}
	"IF false THEN 1 FI" should {
		  "return an error (ELSE missing)" in {
		    interpretTest("else") must beLeft and notBeErrorBefore()
		  }
	}
	
	"DEF minus(a:nat, b:nat) == sub(a,b):nat \nDEF combi(a:nat, b:nat, c:nat, d:nat):nat ==\n\tmul(minus(a,b),add(div(b,c),d))\nDEF isNotSubst(a:nat,b:nat):bool == lt(a,b)\nDEF isDiv(b:nat, c:nat):bool == not(eq(c,0))\nDEF MAIN:nat ==\n\tIF and(not(isNotSubst(9,6)), or(isDiv(6,3), false)) THEN combi(9,6,3,2) ELSE 0 FI" should {
		  "be equal 12" in {
		    interpretTest("allTogether") must beRight(NumV(12))
		  }
	}
	
	
    def testAdd(){
		 "add(8,2)" should {
		  "be equal 10" in {
		    interpretTest("add_1") must beRight(NumV(10))
		  }
		}
		"add(2147483645,21474836)" should {
		  "return error (int overflow)" in {
		    interpretTest("add_2") must beLeft and notBeErrorBefore()
		  }
		}
    }
    
    def testSub(){
		"sub(8,2)" should {
		  "be equal 6" in {
		    interpretTest("sub_1") must beRight(NumV(6))
		  }
		}
		"sub(2,8)" should {
		  "return error (result is < 0)" in {
		    interpretTest("sub_2") must beLeft and notBeErrorBefore()
		  }
		}
    }
    
    
    def testDiv(){
		"div(8,2)" should {
		  "be equal 4" in {
		    interpretTest("div_1") must beRight(NumV(4))
		  }
		}
		"div(9,2)" should {
		  "be equal 4 (int divison)" in {
		    interpretTest("div_2") must beRight(NumV(4))
		  }
		}
		"div(8,0)" should {
		  "return error (division by 0)" in {
		    interpretTest("div_3") must beLeft and notBeErrorBefore()
		  }
    	}
    }
    
    def testMul(){
    	"mul(8,2)" should {
		  "be equal 16" in {
		    interpretTest("mul_1") must beRight(NumV(16))
		  }
		}
		"mul(999999,999999)" should {
		  "return error (int overflow)" in {
		    interpretTest("mul_2") must beLeft and notBeErrorBefore()
		  }
    	}
    }
    
    val err_checker = Diag("ERROR CHECKER", Global)
    val err_scanner = Diag("ERROR SCANNER", Global)
    val err_parser = Diag("ERROR PARSER", Global)
    
    def notBeErrorBefore() = be_!=(Left(err_parser)) and be_!=(Left(err_scanner)) and be_!=(Left(err_checker))
    
    def interpretTest(filename:String) : Either[Diag, Value] =  {
    	val noOpt:Options = new Options(false, false, false, "test")
    	val inputString = fromFile("src/test/resources/"+filename+".mo").mkString
    	val scanRes = Scanner.scan(inputString,noOpt)
    	scanRes match {
    		case Right(ts) => Parser.parse(ts,noOpt) match {
    			case Right(defs) => Checker.check(defs,noOpt) match{
    				case None => Interpreter.interpret(defs,noOpt)
    				case Some(_) => Left(err_checker)
    			}
    			case Left(d) =>  Left(err_parser)
    		}
    		case Left(d) => Left(err_scanner)
    	}
    }
  }
