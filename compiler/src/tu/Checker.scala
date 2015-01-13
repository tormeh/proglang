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
import scala.language.implicitConversions
import scala.language.postfixOps


/** The μ-Opal context checker */
object Checker {

	case class TypeResult(t: NodeType, err: List[Diag] = List())

	case class Context(m: Map[(Scope,String), NodeType]) {  
		def apply(sco:Scope, s: String, pos:Position): TypeResult =
		  if (m.contains((sco,s))) TypeResult(m(sco,s))
		  else if(m.contains((GlobalScope, s))) TypeResult(m(GlobalScope,s))
		  else TypeResult(TyUnknown,List(msg("undefined name " + s, pos)))
		def +(tu: ((Scope, String), NodeType)): Context = {
				Context(m + (tu))
			}
		def ++(l:List[((Scope, String), NodeType)]): Context = {
			var newM = m
			for(elem <- l){
				newM = newM + (elem)
			}
			Context(newM)
			/*RECURSIVE VERSION, bit less optim I think
			l match {
				case Nil => this
				case li => (this + l.head) ++ l.tail
			}*/
    	}
	}

	def typeOf(e: Node, sco:Scope, ctx:Context): TypeResult = typeOf(ctx, e, sco)

  // here we can add information for primitive functions
	val initCtx: Context = new Context(new HashMap[(Scope,String),NodeType]() ++ List(
		((GlobalScope,"add"),TyFun(TyArgs(List(TyNat, TyNat)),TyNat)),
		((GlobalScope,"sub"),TyFun(TyArgs(List(TyNat, TyNat)),TyNat)),
		((GlobalScope,"mul"),TyFun(TyArgs(List(TyNat, TyNat)),TyNat)),
		((GlobalScope,"div"),TyFun(TyArgs(List(TyNat, TyNat)),TyNat)),
		((GlobalScope,"eq"),TyFun(TyArgs(List(TyNat, TyNat)),TyBool)),
		((GlobalScope,"lt"),TyFun(TyArgs(List(TyNat, TyNat)),TyBool)),
		((GlobalScope,"and"),TyFun(TyArgs(List(TyBool, TyBool)),TyBool)),
		((GlobalScope,"or"),TyFun(TyArgs(List(TyBool, TyBool)),TyBool)), 
		((GlobalScope,"not"),TyFun(TyArgs(List(TyBool)),TyBool))))

/* Get all the defintions of function, and return their type as well as their aguments in a new Context
	DOESN'T do anything with the body of functions
*/
  def getBaseContext(initCtx: Context, ast: List[Def], err: List[Diag]): (Context, List[Diag]) =
  {
    if (ast.isEmpty == true) (initCtx, err)
    else
    {
      ast.head.l match
      {
        case MAIN(_, _) => 
        {
          val main = ast.head.l.asInstanceOf[MAIN]     
          if ( initCtx.m.contains((GlobalScope, "MAIN")) ) 
          { 
            val extErr = err :+ Diag("MAIN has multiple definitions", Global)
            val extCtx = initCtx + ((GlobalScope, "MAIN"), TyUnknown) 
            getBaseContext(extCtx, ast.tail, extErr)
          }
          else
          {
            val extCtx = initCtx + ((GlobalScope, "MAIN"), TyFun(TyArgs(List[NodeType]()),main.typ.toNodeType) )
            getBaseContext(extCtx, ast.tail, err)
          }
        }
        case Function_def(_, _, _, pos) =>
        {
          val func = ast.head.l.asInstanceOf[Function_def]
          if ( initCtx.m.contains((GlobalScope, func.name.s)) ) 
          { 
            val extErr = err :+ Diag(func.name.s + " has multiple definitions", Global)
            val functype = ((GlobalScope, func.name.s), TyUnknown)
            /*val argstypectx = func.args.map( param => (initCtx(ScopeFun(func.name.s), param.name.s, pos) match {
               								case TypeResult(TyUnknown, er) => if(er.isEmpty) ((ScopeFun(func.name.s), param.name.s), TyUnknown)
               																  else ((ScopeFun(func.name.s), param.name.s), param.toNodeType)
               								case TypeResult(t, _) => if(t &= param.toNodeType) ((ScopeFun(func.name.s), param.name.s), param.toNodeType)
               														 else ((ScopeFun(func.name.s), param.name.s), TyUnknown)
               								case _ => ((ScopeFun(func.name.s), param.name.s), TyUnknown)
               							}
               						))*/
            val (argstypectx, argserr) = getargstypes(func.args, func.name.s, initCtx, List(), pos)
            val extCtx = Context((initCtx + functype).m ++ argstypectx.m)
             
            getBaseContext(extCtx, ast.tail, extErr ++ argserr)
          }
          else
          {
            val functype = ((GlobalScope, func.name.s), TyFun(TyArgs(func.args.map(x => x.toNodeType)),func.typ.toNodeType))
            //val argstypes = func.args.map( param => ((ScopeFun(func.name.s), param.name.s), param.toNodeType) )
            val (argstypectx, argserr) = getargstypes(func.args, func.name.s, initCtx, List(), pos)
            val extCtx = Context((initCtx + functype).m ++ argstypectx.m)
            
            getBaseContext(extCtx, ast.tail, err++argserr)
          }
        }
        case _ => getBaseContext(initCtx, ast.tail, err :+ Diag("wrong def encountered in getBaseContext", Global))
      }
    }
  }
  
  def getargstypes(params: List[Param], funcname: String, initCtx: Context, err: List[Diag], pos: Position): Tuple2[Context, List[Diag]] =
  {
    if (params.isEmpty)
    {
      (initCtx, err)
    }
    else
    {
      val param = params.head
      val typeargres = initCtx(ScopeFun(funcname), param.name.s, pos)
      if ( initCtx.m.contains((ScopeFun(funcname), param.name.s)) )
      {
        val unk = ((ScopeFun(funcname), param.name.s), TyUnknown)
        val extCtx = initCtx + unk
        val exterr = err :+ Diag("multiple definitions of argument " + params.head.name.s, pos)
        getargstypes(params.tail, funcname, extCtx, exterr, pos)
      }
      else
      {
        val typearg = typeargres match 
        {
          case TypeResult(TyUnknown, er) =>
          {
            if(er.isEmpty) ((ScopeFun(funcname), param.name.s), TyUnknown)
            else ((ScopeFun(funcname), param.name.s), param.toNodeType)
          }
          case TypeResult(t, _) => 
          {
            if(t &= param.toNodeType) ((ScopeFun(funcname), param.name.s), param.toNodeType)
            else ((ScopeFun(funcname), param.name.s), TyUnknown)
          }
          case _ => ((ScopeFun(funcname), param.name.s), TyUnknown)
        }
        val extCtx = initCtx + typearg
        getargstypes(params.tail, funcname, extCtx, err, pos)
      }
    }
    
    
    
  }

/*
	Given a context, return the type of a node of the AST
*/
	private def typeOf(ctx: Context, e: Node, sco:Scope): TypeResult = e match 
	{ 
		case Number(num, pos) => TypeResult(TyNat)
		case Bool(b, pos) => TypeResult(TyBool)
		case Ide(name, pos) => ctx(sco,name, pos)
							
		case Function(name, variables, pos) => 
		{
			val typF = ctx(sco,name.toString(), name.pos)
			typF match {
				case TypeResult(TyFun(args, res), err) => {
					var errVar = err
					var args2 = args.comps
					for (vari <- variables) {
						val position = vari match{
							case Ide(_,pos) => pos
							case Number(_,pos) => pos
							case Bool(_,pos) => pos
							case Function(_, _, pos) => pos
							case ITE(_,_,_,pos) => pos
							case _ => throw new Exception("Bad Param type in typeOf<case Function> at " + pos)
						}

						if(!args2.isEmpty) {
							val TyVari = typeOf(ctx, vari, sco)
							if(!(args2.head &= TyVari.t))	errVar = errVar  :+ msg("variable should be " + args2.head.toString(), position)
							errVar = errVar ++ TyVari.err
							args2 = args2.tail
						}
						else errVar = errVar :+ msg("too many arguments in " + name, pos)
					}
					if(!args2.isEmpty) errVar = errVar :+ msg("not enough arguments in "+ name,pos)
					TypeResult(res, errVar);
				}
					
				case TypeResult(TyUnknown, err) => TypeResult(TyUnknown, err)
				case TypeResult(_, err) => TypeResult(TyUnknown, err :+ msg("function expected", name.pos))
				case _ => TypeResult(TyUnknown, List(msg("Not implemented", name.pos)))
			}
		}
		case ITE(condition, if_true, if_false, pos) =>
		{
			val cond = typeOf(ctx, condition, sco)
			val tr = typeOf(ctx, if_true, sco)
			val fal = if_false match{
				case null => null
				case _ => typeOf(ctx, if_false, sco)
			}
			val errCond:List[Diag] = cond match{
				case TypeResult(TyUnknown,errC) => errC  
				case TypeResult(TyBool,errC) => errC
				case TypeResult(_,errC) => errC :+ msg("Condition must be boolean", pos)
		  	}
			(tr,fal) match{
				case (TypeResult(TyUnknown, errT), null) => TypeResult(TyUnknown, errCond ++ errT)
				case (TypeResult(t1, errT), null) => TypeResult(t1, errCond ++ errT)
				case (TypeResult(TyUnknown,errT),TypeResult(t2,errE)) => TypeResult(t2,errCond ++ errT ++ errE)
				case (TypeResult(t1,errT),TypeResult(t2,errE)) => {
				  if(t1 &= t2){
					TypeResult(t1,errCond ++ errT ++ errE)
				  }
				  else TypeResult(TyUnknown,errCond ++ errT ++ (errE :+ msg("then and else must have same type", pos)))
				}
			  }    
		}
	}
  /**
    * Starts context check for μ-Opal 
    * @param input List of definitions [[Def]]
    * @param opts [[Options]] given as arguments to compiler  ==> Not implemented yet
    * @return A list of error messages
    * */
  def check(input:List[Def],opts:Options):Option[List[Diag]] = {
  
  	//get the initial Context (predefined function + defined function)
  	val (newCtx, errs) = getBaseContext(initCtx, input, List())
  	var res = errs
  	
  	//for each function, verify the return type of the body in regards to the initial Context
  	for (node <- input){
		node match { 
			case Def(l, r) => {
				val (name,position) = l match{ 
					case Function_def(n, _, _, pos) => (n.toString(), pos)
					case MAIN(_, pos) => ("MAIN", pos)
					case _ => throw new Exception("Wrong Definition (not type Main, not type Function)")
				}
				val sco = ScopeFun(name)
				val ty1 = typeOf(newCtx, r, sco)
				val ty2 = newCtx(GlobalScope, name, Global) match{
					case TypeResult(TyFun(_, ret), _) => ret
					case TypeResult(TyUnknown, _) => TyUnknown
					case _ => throw new Exception("Bad Function type for " + name)
				}
				if(!(ty1.t &= ty2)) res = res :+ Diag("function " + name + " should return " + ty2 + " but got " + ty1.t, position)
				if (!ty1.err.isEmpty) res = res ++ ty1.err
			}
			case _ => throw new Exception("Non Definition Entry in input of Context Checker")
		}
    }
    
    //Check if a MAIN is defined
    res = res ++ (newCtx(GlobalScope, "MAIN", Global) match {
    	case TypeResult(TyUnknown, List(Diag("undefined name MAIN", Global))) => List(Diag("MAIN not defined", Global))
    	case TypeResult(TyUnknown, _) => Nil
    	case TypeResult(t, e) => Nil
    }
    )
    if (res.isEmpty) None
    else Some(res.sorted)   
  }

	private def msg(s: String, p:Position):Diag = Diag(s, p) //TODO with position, + " at " + pos
}
