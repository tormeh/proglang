
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

import de.tuberlin.uebb.comp1.machine._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map


/** μ-Opal code generator */

object Coder
{
  type Code = List[Instruction]
  /**
    * Generates [[Code]] for the given list of definitions
    * @param input List of definitions representing the context checked program
    * @param opts [[Options]] given as arguments to comiler  
    * @return Either an error message [[Diag]] or the code for the stack machine
    * */
  def compile(input:List[Def],opts:Options):Either[Diag,Code] =
  {
    //Left(Diag("Coder not yet implemented",Global))
    //Right(List(PushInt(5), Stop))
    
    compileDefs(input) match
    {
      case Right(ins) => 
      {
        println("compiled: " + ins.toString())
        val ins2 = setpushes(ins)
        val ins3 = Jmp(LabeledAddress("MAIN")) :: ins2
        val(lmap,ins4) = wipeLabels(0, ins3)
        val finalCode = wipeRefs(lmap, ins4)
        println("compiled: " + finalCode.toString())
        Right(finalCode)
        //Right(List[Instruction](PushInt(3), PushAddr(Pointer(5)), Call, Slide(1), Stop, Push(1), Ret))
        //Right(List[Instruction](PushAddr(Pointer(2)), Call, PushInt(2), Slide(1), Stop))
        //Right(List[Instruction](Jmp(Pointer(16)), PushInt(0), Push(2), Eq, Jz(Pointer(7)), PushInt(1), Jmp(Pointer(15)), PushInt(1), Push(4), Sub, PushAddr(Pointer(1)), Call, Slide(1), Push(5), Mul, Ret, PushInt(5), PushAddr(Pointer(1)), Call, Slide(1), Stop))
        //Right(List[Instruction](Jmp(Pointer(16)), PushInt(0), Push(2), Eq, Jz(Pointer(7)), PushInt(1), Jmp(Pointer(15)), PushInt(1), Push(4), Sub, PushAddr(Pointer(1)), Call, Slide(1), Push(5), Mul, Ret, PushInt(5), PushAddr(Pointer(1)), Call, Slide(1), Stop))
      }
      case Left(err) => Left(err)
    }
    
   
  }
  
  def compileDefs(input:List[Def]):Either[Diag,Code] =
  {
    if (input.isEmpty)
    {
      Right(List[Instruction]())
    }
    else
    {
      val thisdef = input.head
      println("def")
      println(thisdef.toString())
      println("")
      val start = thisdef.l match
      {
        case MAIN(_,_) => List[Instruction](Label("MAIN"))
        case Function_def(name,vars,_,_) => List[Instruction](Label(name.s)) ++ vars.map(x => Label("arg "+x.name.s))
      }
      val code = compileExpr(thisdef.r)
      val end = thisdef.l match
      {
        case MAIN(_,_) => List[Instruction](Stop)
        case Function_def(name,_,_,_) => List[Instruction](Ret)
      }
      compileDefs(input.tail) match
      {
        case Right(next) => Right(start ++ code ++ end ++ next)
        case Left(err) => Left(err)
      }
      
    }
  }
  
  def compileExpr(expr: Expr): List[Instruction] =
  {
    if (expr == null)
    {
      return List[Instruction](Abort("check failed"))
    }
    expr match
    {
      case Function(name,vars,_) => 
      { 
        name.s match
        {
          case "add" => compileExpr(vars(1)) ++ compileExpr(vars(0)) ++ List[Instruction](Add)
          case "mul" => compileExpr(vars(1)) ++ compileExpr(vars(0)) ++ List[Instruction](Mul)
          case "eq" => compileExpr(vars(1)) ++ compileExpr(vars(0)) ++ List[Instruction](Eq)
          case "sub" => compileExpr(vars(1)) ++ compileExpr(vars(0)) ++ List[Instruction](Sub)
          case "div" => compileExpr(vars(1)) ++ compileExpr(vars(0)) ++ List[Instruction](Div)
          case "lt" => 
          {
            val label1 = labelmachine.getlabel()
            val label2 = labelmachine.getlabel()
            compileExpr(vars(1)) ++ compileExpr(vars(0)) ++ List[Instruction](Sub, Jlt(LabeledAddress(label1)), PushInt(0), Jmp(LabeledAddress(label2)), Label(label1), PushInt(1), Label(label2))
          }
          case "and" => compileExpr(vars(1)) ++ compileExpr(vars(0)) ++ List[Instruction](Add, PushInt(2), Eq)
          case "or" => compileExpr(vars(1)) ++ compileExpr(vars(0)) ++ List[Instruction](Add, PushInt(0), Eq, PushInt(0), Eq)
          case "not" => compileExpr(vars(0)) ++ List[Instruction](PushInt(0), Eq)
          case _ =>
          {
            val args = vars.reverse.map(x => compileExpr(x)).foldLeft(List[Instruction]())(_++_): List[Instruction]
            val callins = List[Instruction](PushAddr(LabeledAddress(name.s)), Call, Slide(vars.length))
            args ++ callins
          }
          
        }
      }
      case Number(n,_) => List[Instruction](PushInt(n))
      case Bool(b,_) => List[Instruction](b match { case true => PushInt(1); case false => PushInt(0) })
      case ITE(condition:Expr, if_true:Expr, if_false:Expr,_) => 
      {
        val label1 = labelmachine.getlabel()
        val label2 = labelmachine.getlabel()
        val (opts1, opte1) = labelmachine.getlabeloptional()
        val (opts2, opte2) = labelmachine.getlabeloptional()
        compileExpr(condition) ++ List[Instruction](Jz(LabeledAddress(label1)), Label(opts1)) ++ compileExpr(if_true) ++ List[Instruction](Label(opte1), Jmp(LabeledAddress(label2))) ++ List[Instruction](Label(label1), Label(opts2)) ++ compileExpr(if_false) ++ List[Instruction](Label(opte2), Label(label2))
      }
      case Ide(name,_) => List[Instruction](Label("push "+name))
      case ITE(condition:Expr, if_true:Expr, null,_) => 
      {
        val label1 = labelmachine.getlabel()
        val label2 = labelmachine.getlabel()
        val (opts1, opte1) = labelmachine.getlabeloptional()
        val (opts2, opte2) = labelmachine.getlabeloptional()
        compileExpr(condition) ++ List[Instruction](Jz(LabeledAddress(label1)), Label(opts1)) ++ compileExpr(if_true) ++ List[Instruction](Label(opte1), Jmp(LabeledAddress(label2))) ++ List[Instruction](Label(label1), Label(opts2)) ++ compileExpr(null) ++ List[Instruction](Label(opte2), Label(label2))
      }
    }
  }
  
  def removelabels(instructions: List[Instruction]): List[Instruction] =
  { //Label(MAIN), PushInt(3), PushInt(2), Sub, Jlt(LabeledAddress(label1)), PushInt(0), Jmp(LabeledAddress(label2)), Label(label1), PushInt(1), Label(label2), Stop
    println("")
    println("starting removelabels")
    println(instructions.toString())
    println("")
    var positions = ListBuffer[Int]()
    var i = -1
    for (x <- instructions)
    {
      x match
      {
        case Label(_) => i=i
        case _ => i=i+1
      }
      positions += i
    }
    println(positions.toString())
    instructions
  }
  
  def wipeLabels(line: Int, code :Code) : (Map[String, Int],Code) = code match
    {
      case (Label(s)::rs) =>
        val (lmap, wcode) = wipeLabels(line,rs)
        (lmap + (s -> line), wcode)
      case (x :: rs) =>
        val (lmap, wcode) = wipeLabels(line+1,rs)
        (lmap, x::wcode)
      case Nil => (Map(), List())
    }
    
    def wipeRefs(lmap: Map[String,Int], code : Code) : Code =
    code.map({ instr => instr match
    {
      case PushAddr(LabeledAddress(l)) => PushAddr(Pointer(lmap(l)))
      case Jmp(LabeledAddress(l)) => Jmp(Pointer(lmap(l)))
      case Jz(LabeledAddress(l)) => Jz(Pointer(lmap(l)))
      case Jlt(LabeledAddress(l)) => Jlt(Pointer(lmap(l)))
      case Jgt(LabeledAddress(l)) => Jgt(Pointer(lmap(l)))
      case i => i
    }})
    
    def setpushes(ins: List[Instruction]): List[Instruction] =
    {
      //Label(MAIN), PushInt(3), PushAddr(LabeledAddress(func)), Call, Stop, Label(func), Label(x), Label(push x), Ret
      println("")
      println("starting setpushes")
      println(ins.toString())
      println("")
      
      var mutins = ins.to[ListBuffer]
      
      var i = 0
      var stackdepth = List[(Int,Map[String,Int])] ((0,Map[String,Int]()))
      var currentfunction = ""
      var initialdepth = 0
      
      for(x <- 0 to (ins.length-1))
      {
        ins(x) match
        {
          case Label(str) =>
          {
            if (str.contains("push "))
            {
              mutins(x) = Push(stackdepth.head._2(str.stripPrefix("push ")))
              stackdepth.head._2.foreach(x => stackdepth.head._2(x._1)+=1)
            }
            else if (str.contains("arg "))
            {
              initialdepth += 1
              stackdepth.head._2 += (str.stripPrefix("arg ") -> initialdepth)
            }
            else if (str.contains("optionalpathstart "))
            {
              var newhead = (str.stripPrefix("optionalpathstart ").toInt,Map[String,Int]())
              stackdepth.head._2.foreach(x => newhead._2 += x)
              stackdepth = newhead :: stackdepth
            }
            else if (str.contains("optionalpathend "))
            {
              if (str.stripPrefix("optionalpathend ").toInt == stackdepth.head._1)
              {
                stackdepth = stackdepth.tail
              }
              else {
                println("FUCKFUCKFUCK")
              }
            }
            else if (!str.contains("label ")) 
            {
              //it's then a function and we reboot the process
              stackdepth = List[(Int,Map[String,Int])] ((0,Map[String,Int]()))
              initialdepth = 0
            }
          }
          case Add => {stackdepth.head._2.foreach(x => stackdepth.head._2(x._1)-=1)}
          case Div => {stackdepth.head._2.foreach(x => stackdepth.head._2(x._1)-=1)}
          case Eq => {stackdepth.head._2.foreach(x => stackdepth.head._2(x._1)-=1)}
          case Mul => {stackdepth.head._2.foreach(x => stackdepth.head._2(x._1)-=1)}
          case Sub => {stackdepth.head._2.foreach(x => stackdepth.head._2(x._1)-=1)}
          case Jgt(_) => {stackdepth.head._2.foreach(x => stackdepth.head._2(x._1)-=1)}
          case Jlt(_) => {stackdepth.head._2.foreach(x => stackdepth.head._2(x._1)-=1)}
          case Jz(_) => {stackdepth.head._2.foreach(x => stackdepth.head._2(x._1)-=1)}
          case PushInt(_) => {stackdepth.head._2.foreach(x => stackdepth.head._2(x._1)+=1)}
          case PushAddr(_) => {stackdepth.head._2.foreach(x => stackdepth.head._2(x._1)+=1)}
          case Slide(amountremoved) => {stackdepth.head._2.foreach(x => stackdepth.head._2(x._1)-=amountremoved)}
          case _ => {}
        }
      }
      val out = mutins.toList
      println("")
      println("finished setpushes")
      println(out.toString())
      println("")
      out
    }
}

    
object labelmachine
{
  private var i = 0
  def getlabel(): String =
  {
    i = i+1
    "label "+i.toString()
  }
  def getlabeloptional(): (String, String) =
  {
    i = i+1
    ( "optionalpathstart "+i.toString(), "optionalpathend "+i.toString())
  }
}





