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

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
 */

package de.tuberlin.uebb.comp1.homework



/**
  * Represents a definition, whole program is a list of definitions
  * */

abstract class Node() {
}

case class Def(l: Lhs, r:Expr) extends Node

case class Param(name:Ide, typ:Type)
{
  def toNodeType = typ.toNodeType
}

abstract class Lhs() extends Node
case class Function_def(name:Ide, args:List[Param], typ:Type, pos:Position) extends Lhs
case class MAIN(typ:Type, pos:Position) extends Lhs

case class Type(name:String, pos:Position) extends Node
{
  def toNodeType = name match
  {
    case "nat" => TyNat
    case "bool" => TyBool
    case _ => throw new IllegalArgumentException("wrong string in Type.toNodeType")
  }
}

abstract class Expr() extends Node{

}
case class Function(name:Ide, variables:List[Expr], pos:Position) extends Expr
case class Number(n:Int, pos:Position) extends Expr
case class Bool(b:Boolean, pos:Position) extends Expr
case class Ide(s:String, pos:Position) extends Expr {
	override def toString():String = s
}
case class ITE(condition:Expr, if_true:Expr, if_false:Expr, pos:Position) extends Expr		//If Then Else

abstract class NodeType 
{

    def &=(t2: NodeType): Boolean = (this, t2) match 
    {
      case (TyUnknown, _) => true
      case (_, TyUnknown) => true
      case (x, y) => x == y
    }
}

case object TyNat extends NodeType
{
   override def toString() = "nat"
}
case object TyBool extends NodeType
{
   override def toString() = "bool"
}
case object TyUnknown extends NodeType
case class TyArgs(comps: List[NodeType]) extends NodeType
case class TyFun(arg: TyArgs, res: NodeType) extends NodeType{
   override def toString() = "(" ++ arg.toString ++ ") -> (" ++ res.toString ++ ")" 
}


