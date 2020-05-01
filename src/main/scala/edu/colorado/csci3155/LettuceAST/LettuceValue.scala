package LettuceAST

import LettuceAST.LettuceInterpreter.evalExprCPS

sealed trait Value

case class NumValue(f: Double) extends Value
case class BoolValue(b: Boolean) extends Value
case class Closure(argList: List[String], e: Expr, sigma: LettuceEnvironment) extends Value
case class Reference(k: Int) extends Value

// New class
case class BreakValue( e: Expr, env: LettuceEnvironment, st: LettuceStore,
                       currN: Int, breakN: Int) extends Value

object  LettuceValue {

    def valueToNumCPS[T](v: Value, st: LettuceStore, k: (Double, LettuceStore) => T): T = v match {
        case NumValue(d) =>
            //            println(s"v->N($d): v:$v\n")
            k(d, st)

        case Reference(j) =>
            val x = StoreInterface.mkDeref(j, st)
            x match {
                case NumValue(d) => k(d, st)
            }


        case  _ =>
            println(s"(Not NumValue in valueToNum(v->N($v, ${v.getClass}): v:$v\n")
//            k(2, st)
            throw new IllegalArgumentException(s"Error: Asking me to convert Value: $v to a number")
    }

    def valueToBoolCPS[T](v: Value, st: LettuceStore, k: (Boolean, LettuceStore) => T): T = v match {
        case BoolValue(b) => k(b, st)
        case _ => throw new IllegalArgumentException(s"Error: Asking me to convert Value: $v to a boolean")
    }


    def valueToClosureCPS[T](v: Value, st: LettuceStore, k: (Value, LettuceStore) => T): T = v match {
        case Closure(x, e, pi) => k(Closure(x, e, pi), st)

        case _ => throw new IllegalArgumentException(s"Error: Asking me to convert Value: $v to a closure\n\t(v: ${v.getClass()}, st: $st\n")
    }

    def valueToReferenceCPS[T](v: Value, st: LettuceStore, k: (Int, LettuceStore) => T): T = v match {
        case Reference(j) => k(j, st)
        case anyCase @ _ =>
            println(s"valToRefCPS: v: $v, ${v.getClass()}, case $anyCase : ${anyCase.getClass()}\n")
            throw new TypeConversionError("Converting from non reference to a reference value")

    }

}
