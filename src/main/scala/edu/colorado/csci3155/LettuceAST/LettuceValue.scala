package LettuceAST

sealed trait Value

case class NumValue(f: Double) extends Value
case class BoolValue(b: Boolean) extends Value
case class Closure(argList: List[String], e: Expr, sigma: LettuceEnvironment) extends Value
case class Reference(k: Int) extends Value
// new break value class
case class BreakValue( e: Expr, env: LettuceEnvironment, st: LettuceStore, currN: Int, breakN: Int) extends Value
//case class BreakValueRec(currN: Int, e: Expr, env: LettuceEnvironment, st: LettuceStore) extends Value

object  LettuceValue {

//    def valueToNum[T](v: Value, k: Value => T): T =  v match {
//        case NumValue(f) => k(f)
//
//        case _=> {
//            println(s"Error: Type error (Nan => NUM VALUE) v: $v")
//            throw new TypeConversionError("Converting from non numeric to number value")
//        }
//
////        case _ =>  throw new TypeConversionError("Converting from non numeric to number value")
//    }
//
//    def valueToBool(v: Value): Boolean = v match  {
//        case BoolValue(b) => b
//        case _ => throw new TypeConversionError("Converting from non boolean to boolean value")
//    }
//
//    def valueToClosure(v: Value): Closure = v match  {
//        case Closure(aList, e, sigma) => Closure(aList, e, sigma)
//        case _ => throw new TypeConversionError("Converting from non closure to a closure value")
//    }
//
//    def valueToReference(v: Value): Int =  v match {
//        case Reference(k) => k
//        case _ => throw new TypeConversionError("Converting from non reference to a reference value")
//    }


    def valueToNumCPS[T](v: Value, st: LettuceStore, k: (Double, LettuceStore) => T): T = v match {
        case NumValue(d) => {
            println(s"v->N($d): v:$v\n")
            k(d, st)
        }

        case Reference(j) => {
            val x = StoreInterface.mkDeref(j, st)
            x match {
                case NumValue(d) => k(d, st)
            }

        }
        case d @ _ => {
            println(s"v->N($d): v:$v\n")
            k(2, st)
//            throw new IllegalArgumentException(s"Error: Asking me to convert Value: $v to a number")
        }
        }

    def valueToBoolCPS[T](v: Value, st: LettuceStore, k: (Boolean, LettuceStore) => T): T = v match {
        case BoolValue(b) => k(b, st)
        case _ => throw new IllegalArgumentException(s"Error: Asking me to convert Value: $v to a boolean")
    }

    def valueToClosureCPS[T](v: Value,st: LettuceStore, k: (Closure, LettuceStore) => T): T = v match {
        case Closure(x, e, pi) => k(Closure(x, e, pi), st)
        case _ =>  throw new IllegalArgumentException(s"Error: Asking me to convert Value: $v to a closure")
    }

    def valueToReferenceCPS[T](v: Value, st: LettuceStore, k: (Int, LettuceStore) => T): T =  v match {
        case Reference(j) => k(j, st)
        case b @ BreakValue(e, env, st, currN, breakN) => {
            println(s"valToRefCPS-Bv: v: $v, ${v.getClass()}, case $b : ${b.getClass()}\n")
            throw new TypeConversionError("Converting from BREAKVALUE reference to a reference value")
        }

        case anyCase @ _ => {
            println(s"valToRefCPS: v: $v, ${v.getClass()}, case $anyCase : ${anyCase.getClass()}\n")
            throw new TypeConversionError("Converting from non reference to a reference value")
        }
    }

//    def valueToReferenceCPS(v: Value) : Int =  v match {
//        case Reference(j) => j
//        case anyCase @ _ => {
//            println(s"valToRefCPS: v: $v, ${v.getClass()}, case $anyCase : ${anyCase.getClass()}\n")
//            throw new TypeConversionError("Converting from non reference to a reference value")
//        }
//    }

}
