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
import scala.collection.immutable.{HashMap}


/** μ-Opal interpreter */
object Interpreter{

	val primitives = List("add","sub","mul","div","eq","lt","or", "and", "not")
	
	private def isPrimitiveFunction(name:String):Boolean = {
		primitives.contains(name)
	}
	
	private def applyPrimitiveFunction(name:String,a1:Either[Int, Boolean],a2:Either[Int, Boolean], pos:Position):Value = {
		name match {
			case "add" => {	
				(a1,a2) match{
					case (Left(a), Left(b)) => {
						if(a + b < 0) Undef(Diag("Integer overflow, Max Integer reached", pos)) //not really good, but works
						else NumV(a + b)
					}
					case _ => throw new Exception("add applied to non integers")
				}							 
			}
			case "sub" => {
				(a1,a2) match{
					case (Left(a), Left(b)) => {
						if(a - b < 0) Undef(Diag("Negative result is not a natural", pos))
						else NumV(a - b)
					}
					case _ => throw new Exception("sub applied to non integers")
				}			
			}
			case "mul" => {
				(a1,a2) match{
					case (Left(a), Left(b)) => {
						var result:Either[NumV, Undef] = Left(NumV(0));
						try{
							val value = a.toLong * b.toLong
							if(value > Int.MaxValue || value < Int.MinValue) result = Right(Undef(Diag("Integer overflow, Max Integer reached", pos)))
							else result =  Left(NumV(value.toInt))
						}
						catch{
							case e:NumberFormatException =>	result = Right(Undef(Diag("Integer overflow, Max Integer reached", pos)))
							case _ : Throwable => throw new Exception("Unexpected input for integer")
						}
						result match{
							case Left(l) => l
							case Right(r) => r
						}
					}
					case _  => throw new Exception("mul applied to non integers")
				}
			}	
			case "div" => {
				(a1,a2) match{
					case (Left(a), Left(b)) => {
						if(b == 0) Undef(Diag("Cannot divide by 0", pos))
						else NumV(a / b)
					}							 
					 case _ => throw new Exception("div applied to non integers")
				 }
			}
			case "eq" => {
				(a1,a2) match{
					case (Left(a), Left(b)) => BoolV(a == b)
					case _ => throw new Exception("eq applied to non integers")
				}
			}
			case "lt" => {
				(a1,a2) match{
					case (Left(a), Left(b)) => BoolV(a < b)
					case _ => throw new Exception("lt applied to non integers")
				}
			}
			case "and" => {
				(a1,a2) match{
					case (Right(a), Right(b)) => BoolV(a && b)
					case _ => throw new Exception("and applied to non booleans")
				}
			}
			case "or" => {
				(a1,a2) match{
					case (Right(a), Right(b)) => BoolV(a || b)
					case _ => throw new Exception("or applied to non booleans")
				}
			}
			case "not" => {
				(a1,a2) match{
					case (Right(a), null) => BoolV(!a)
					case _ => throw new Exception("not applied to non booleans")
				}
			}
		}
	}
	
	def eval(e:Expr, functions:Map[String, (List[String], Expr)]) : Value = eval(Map(), functions, e)
	
	def eval(env:Map[String,Value], functions:Map[String, (List[String], Expr)], e : Expr) : Value = e match {
		case Bool(v,_) => BoolV(v)
		case Number(n,pos) => if(n<0) Undef(Diag("A natural number cannot be lower than 0 (between 0 or bigger than "+Int.MaxValue + ")",pos))
							// else if(n > Int.MaxValue) Undef(Diag("A natural number cannot be greater than 0",pos))  /* How can we catch overflow  ??? */
							else NumV(n)
		case Ide(name,_) => env(name)
		case ITE(cond, th, el, pos) => {
			val cc = eval(env, functions, cond)
      		cc match {
        		case BoolV(true) => eval(env, functions, th)
        		case BoolV(false) => if(el == null){
        				Undef(Diag("Else missing",pos))
        			}else{
        			eval(env, functions, el)
        		}
        		case Undef(d) => Undef(d)
      		}
		}
		case Function(name, args, pos) => {
			//println("function " + "name " + name.s + " with args " + args)
			if(isPrimitiveFunction(name.s)){
				val (args1, argsTail) = (eval(env, functions, args.head), args.tail)
				val args2 = argsTail match {
					case Nil => null
					case _ => eval(env, functions, argsTail.head)
				}
				(args1, args2) match
				{
					case (NumV(a1), NumV(a2)) => applyPrimitiveFunction(name.s,Left(a1),Left(a2), pos)
					case (BoolV(a1), BoolV(a2)) => applyPrimitiveFunction(name.s,Right(a1),Right(a2), pos)
					case (NumV(a1), null) => applyPrimitiveFunction(name.s,Left(a1),null, pos)  
					case (BoolV(a1), null) => applyPrimitiveFunction(name.s,Right(a1),null, pos)   
					case (Undef(d), _) => Undef(d)
					case(_, Undef(d)) => Undef(d)
					case _ => Undef(Diag("function " + name.s + " called with bad parameters", pos))
				}
				
			}
			else{
				var newEnv = env
				var (argsName, body) = functions(name.s)
				for(arg <- args){
					val add = (argsName.head, eval(env, functions, arg))
					if(add._2.isInstanceOf[Undef]) return add._2
					newEnv = newEnv + add
					argsName = argsName.tail
				}
				eval(newEnv, functions, body)
			}
		}
	}
/**
  * Interprets the given list of definitions [[Def]] starting with main function
  * @param input List of definitions representing the context checked program
  * @param opts [[Options]] given as arguments to comiler 
  * @return Either an error message [[Diag]] or a [[Value]]
  * */
  def interpret(input:List[Def],opts:Options):Either[Diag,Value] = {
  	var functions : Map[String, (List[String], Expr)] = new HashMap[String, (List[String], Expr)]()
  	var e:Def = null
  	for(inp <- input){
  		inp.l match{
  			case Function_def(name, args,_,_) => {
  				val add = (name.s, (args.map(p => p.name.s), inp.r))
  				functions = functions + add
  			}
			case MAIN(_, _) => e = inp
  		}
  	}
  	val res = eval(e.r, functions)

  	res match {
  		case Undef(d) => Left(d)
  		case v => Right(v)
  	}
  }
}
