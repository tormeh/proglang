/*
 * Copyright (c) 2013, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS """AS IS""" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * */

package de.tuberlin.uebb.comp1.homework
import scala.collection.mutable.ListBuffer


/** Parser for μ-Opal-Compiler*/
object Parser{

  /** The input element of this parser is a list of [[Token]]*/
  type InputElem = Token

  /** Starts the parser
    * 
    * @param inp the token sequence (result of scanner)
    * @param opts [[Options]] given as arguments to comiler
    * @return either an error message [[Diag]] or a list of definitions [[Def]] 
    * Prog ::= Def Prog1 # 
    * so first step is call the function parsing Prog
    * (fast) Every parses function return the same thing:
    *  - A couple (Token, String) representing an error, like: wrong Token, expected "..." (String)
    *  - A List[Token], rest of the tokens not yet parsed
    *  - A Node, representing a node of the Abstract Syntax Tree (this last element can vary)
    **/

  def parse(inp: List[InputElem],opts:Options): Either[Diag,List[Def]] = {
    val (err, inp2, tree) = parseProg(inp)
    if (err == null)
    {
        println("The tree is: " + tree.toString())
        Right(tree)
    }
    else { Left(syntaxErrorMessage(err)) }
  }
  
  /**
  * Prog ::= Def Prog1 # 
  * so after parsing Prog1, we check if the token rest is EoF. If not, display error
  **/
  def parseProg(inp: List[InputElem]): Tuple3[Tuple2[InputElem, String], List[InputElem], List[Def]] =
  {
    val (err, inp2, defNode) = parseDef(inp)
   	if (err == null)
   	{
   	    val (err2, inp3, progNode) = parseProg1(inp2)
   	    if (err2 == null)
   	    {
   	        inp3.head match
   	        {
   	            case EofT() => (null, null, List(defNode) ++ progNode)
   	            case _ => ((inp.head, "endOfFile"), null, null)
   	        }
   	    }
   	    else { (err2, null, null) }
        
   	}
    else { (err, null, null) }
  }
  
  /**
  * Prog1 ::= Def Prog1       -- starts with DEF
  *           | e             -- starts with EoF
  * , we check the first element, and call the good parser or display error
  **/
  def parseProg1(inp: List[InputElem]): Tuple3[Tuple2[InputElem, String], List[InputElem], List[Def]] =
  {
    inp.head match
    {
    	case DefT() =>
    	{
    	    val (err, inp2, defNode) = parseDef(inp)
    	    if(err == null){
            val (err2, inp3, deflistin) = parseProg1(inp2)
            if(err2 == null){
              val deflist = deflistin :+ defNode
              (null, inp3, deflist)
            }
            else { (err2, null, null) }
          }
          else { (err, null, null) }
    	}
    	case EofT() => (null, inp, List[Def]())
    	case _ => ((inp.head, "Definition"), null, null)
    }
  }
  
  /**
  * Def ::= DEF Lhs == Expr        -- starts with DEF (already checked in Prog1, so we dont verify it again)
  * we parse Lhs, verify that the next token is '==', parse Expr, and return a new Def element
  **/
  def parseDef(inp: List[InputElem]): Tuple3[Tuple2[InputElem, String], List[InputElem], Def] =
  {
    val (err, inp2, lhsNode) = parseLhs(inp.tail)
    if(err == null)
    {
      inp2.head match
      {
         case DefAsT() =>
         {
            val (err2, inp3, exprNode) = parseExpr(inp2.tail)
            if (err2 == null) { (null, inp3, new Def(lhsNode, exprNode)) }
            else { (err2, null, null) }
         }
         case _ => { ((inp2.head, "=="), null, null) }
      }
    }
    else { (err, null, null) }
  }
  
  /**
  * Lhs ::= MAIN : Type          -- starts with MAIN ==> skip MAIN, the ':' Parse Type and return a new element MAIN(type)
  *         | id ( A ) : Type    -- starts with Variable ==> create a new element function_def (definition of a function)
  *                                                          with name id, argument ParseA and type ParseTye we check also '(', ')' and ':' inbetween
  **/
  def parseLhs(inp: List[InputElem]): Tuple3[Tuple2[InputElem, String], List[InputElem], Lhs] =
  {
    inp.head match
    {
        case MainT() => 
        {
            inp.tail.head match
            {
                case ColonT() =>
                {
                    val (err, inp2, typeNode: Type) = parseType(inp.tail.tail)
                    if (err == null) { (null, inp2, new MAIN(typeNode)) }
                    else { (err, null, null) }
                }
                case _ => ((inp.tail.head, ":"), null, null)
            } 
        }
        case VarT(_) => 
        {
            val name: Ide = Ide(inp.head.toString())
            inp.tail.head match
            {
                case OpenT() => 
                {
                    val (err, inp2, args) = parseA(inp.tail.tail)
                    if (err == null)
                    {
                        inp2.head match
                        {
                            case CloseT() => 
                            {
                                inp2.tail.head match
                                {
                                    case ColonT() => 
                                    {
                                        val (err, inp3, typ) = parseType(inp2.tail.tail)
                                        if(err == null) { (null, inp3, Function_def(name, args, typ)) }
                                        else (err, null, null)
                                    }
                                    case _ => ((inp2.tail.head, ":"), null, null)
                                }
                            }
                            case _ => ((inp2.head, ")"), null, null)
                        }
                    }
                    else { (err, null, null) }
                    
                }
                case _ => ((inp.tail.head, "("), null, null)
            }
        }
        case _ => ((inp.head, "Function or MAIN"), null, null)
    } 

  }
  
  /**
  * Expr ::= number      -- starts with Num ==> return an element Number
  *         | true       -- starts with 'true' ==> return an elemen Bool with value true
            | false      -- starts with 'false' ==> return an elemen Bool with value false
            | id C       -- starts with variable ==> return a new value C or function with function(args) with args coming from parseC
            | if Expr then Expr E fi      -- starts with 'if' ==> return a new element ITE (If Then Else) with TE coming from parseE
  *                                                          with name id, argument ParseA and type ParseTye we check also '(', ')' and ':' inbetween
  **/
  def parseExpr(inp: List[InputElem]): Tuple3[Tuple2[InputElem, String], List[InputElem], Expr] =
  {
  	inp.head match
  	{
  	    case NumT(_) => return (null, inp.tail, Number(inp.head.toString().toInt))
  	    case TrueT() => return (null, inp.tail, Bool(true))
  	    case FalseT() => return (null, inp.tail, Bool(false))
  	    case VarT(_) => 
  	    {
  	      var minp = inp.to[ListBuffer]
  	      val name = new Ide(minp.head.toString())
  	      minp = minp.tail
  	      minp.head match{
  	        case OpenT() => {
  	            val(err, minp2, args) = parseC(minp.toList)
  	            if(err == null)
  	            {
  	                (null, minp2, Function(name, args))
  	            }
  	            else { (err, null, null) }
  	        }
  	        case _ => (null, minp.toList, name)
  	      }
  	    }
  	    case IfT() => 
  	    {
  	      val (err, inp2, ifexprnode) = parseExpr(inp.tail)
  	      if (err == null) 
  	      {
  	        inp2.head match
  	        {
  	          case ThenT() =>
  	          {
  	            val (err2, inp3, thenexprnode) = parseExpr(inp2.tail)
  	            if (err2 == null)
  	            {
  	              val (err3, inp4, enode) = parseE(inp3)
  	              if (err3 == null)
  	              {
  	                inp4.head match
  	                {
  	                  case FiT() => (null, inp4.tail, ITE(ifexprnode, thenexprnode, enode))
  	                  case _ => ((inp4.head, "FI"), null, null)
  	                }
  	              }
  	              else { (err3, null, null) }
  	            }
  	            else { (err2, null, null) }
  	          }
  	          case _ => ((inp2.head, "Then"), null, null)
  	        }
  	      }
  	      else { (err, null, null) }
  	      
  	      
  	      
  	    }
  	    case _ => return ((inp.head, "Number, True, False, Variable or IF"), null, null)
  	}
  }
  
 /**
  * C ::= (Expr D)  
  *       | e
  **/
  def parseC(inp: List[InputElem]): Tuple3[Tuple2[InputElem, String], List[InputElem], List[Expr]] = 
  {
    var minp = inp.to[ListBuffer]
    minp.head match{
            case OpenT() => {
                var args = ListBuffer[Expr]()
                minp = minp.tail
                val (err, inp2, e) = parseExpr(minp.toList)
                if (err == null){
                    args += e
                    minp = inp2.to[ListBuffer]
                        val (err2, inp3, e2) = parseD(minp.toList)
                            if(err2 == null){
                                args = args ++ e2
                                
                            }
                            else { (err2, null, null) }
                            
                            inp3.head match 
                            {
                                    case CloseT() => {
                                        (null, inp3.tail, args.toList)
                                    }
                                    case _ => {
                                        ((inp3.head, ")"), null, null)
                                    }
                            }
                }
                else { (err, null, null) }              
            }
  	        case ThenT() | ElseT() | FiT() | CloseT() | DefT() | EofT() =>{
  	          (null, inp, List[Expr]())
  	        }
  	        case _ => {
  	          ((minp.head, "), then, else, fi, DEF, end of file"), null, null)
  	        }
    }
  }
  
  /**
  * D ::= , Expr D  
  *       | e
  **/
  def parseD(inp: List[InputElem]): Tuple3[Tuple2[InputElem, String], List[InputElem], List[Expr]] = 
  {
    inp.head match{
  	        case CommaT() => {
  	          var args = ListBuffer[Expr]()
  	          val(err, inp2, e) = parseExpr(inp.tail)
  	          if(err == null){
  	            args = args ++ List(e)
  	            val(err2, inp3, e2) = parseD(inp2)
  	            if(err2 == null){
  	                 args = args ++ e2
  	                 (null, inp3, args.toList)
  	            }
  	            else { (err2, null, null) }
  	          }
  	          else { (err, null, null) }
  	        }
  	         
  	        case _ =>{
  	          (null, inp, List[Expr]())
  	        }
    }
  }
  
  /**
  * E ::= ELSE Expr  
  *       | e
  **/
  def parseE(inp: List[InputElem]): Tuple3[Tuple2[InputElem, String], List[InputElem], Expr] =
  {
    inp.head match
  	{
  	   case ElseT() => 
  	   {
  	     val (err, inp2, exprnode) = parseExpr(inp.tail) //exprnode may be null!!
  	     if (err == null) { (null, inp2, exprnode) }
  	     else { (err, null, null) }
  	   }
  	   case _ => (null, inp, null)
  	}
  }
  	    
  
  def parseType(inp: List[InputElem]): Tuple3[Tuple2[InputElem, String], List[InputElem], Type] =
  {
    inp.head match
    {
      case NatT() => 
      {
        (null, inp.tail, Type(inp.head.toString()))
      }
      case BoolT() => 
      {
        (null, inp.tail, Type(inp.head.toString()))
      }
      case _ => 
      {
        ((inp.head, "Natural number or Boolean"), null, null)
      }
    }
  }
  
  def parseA(inp: List[InputElem]): Tuple3[Tuple2[InputElem, String], List[InputElem], List[Param]] =
  {
  	var result = ListBuffer[Param]()
 
    inp.head match
    {
        case VarT(_) => 
        {
            val name = Ide(inp.head.toString())
            val inp2 = inp.tail
               
            inp2.head match
            {
                case ColonT() => 
                {
                    var(err2, inp3, typ) = parseType(inp2.tail)
                    if(err2 == null) {
                      result += Param(name, typ)
                      val(err3, inp4, args) = parseB(inp3)
                      if(err3 == null){
                        result = result ++ args
                        (null, inp4, result.toList)
                      }
                      else { (err3, null, null) }
                    }
                    else { (err2, null, null) }        
                }
                case _ => ((inp2.head, ":"), null, null)
            }                                           
        }
        case CloseT() => { (null, inp, List[Param]()) }
        case _ => 
        {
          ((inp.head, "Variable"), null, null)
        }
    }
  }
  
  def parseB(inp: List[InputElem]): Tuple3[Tuple2[InputElem, String], List[InputElem], List[Param]] = 
  {
    var result = ListBuffer[Param]()
    
    inp.head match {
      case CommaT() => {
        val inp2 = inp.tail
        inp2.head match {         
          case VarT(_) => {
            val name = Ide(inp2.head.toString())
            val inp3 = inp2.tail
            
            inp3.head match
            {
                case ColonT() => 
                {
                  var(err, inp4, typ) = parseType(inp3.tail)
                  if(err == null) {
                    result += Param(name, typ)
                    val(err2, inp5, e) = parseB(inp4)
                    if(err2 == null) 
                    { 
                      result = result ++ e
                      (null, inp5, result.toList)
                    }
                    else { (err2, null, null) }
                  }
                  else { (err, null, null) }
                }
                case _ => {
                  ((inp3.head, ":"), null, null)
                }
            }
          }
          case _ => {
            ((inp2.head, "Variable"), null, null)
          }
        }
      }
      case CloseT() =>{
        (null, inp, List[Param]())
      }
      case _ => {
        ((inp.head, ")"), null, null)
      }
    }
              
     
  }
  
  /**
  * Display an error from a faulty InputElem and (an) expected value(s) as a String 
  * ex: Syntax Error: expected ( at position... but got 3 
  **/
  def syntaxErrorMessage(err: Tuple2[InputElem, String]): Diag =
  {
    val (faultyToken, tokenname) = err
  	val tokenname2 = faultyToken.toString()
  	val tokenposition = faultyToken.getPosition.toString()
  	
  	Diag("Syntax error: Expected " + tokenname + " at " + tokenposition + " but got " + tokenname2 , Global)
  }
}




















