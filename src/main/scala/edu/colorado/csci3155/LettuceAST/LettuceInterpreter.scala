package LettuceAST

import LettuceAST.LettuceInterpreter.evalExprCPS


case object LettuceInterpreter {
    def evalProgram(p: Program, breakN: Int, pB: Program): Value = {

        //        val k = (v: Value, s: StoreInterface) => {(v, s)}
        p match {
            //        case TopLevel(BreakValue(c, eB, envB, stB)) => {
            //            throw new IllegalArgumentException(s"BreakValue: at a 'Top Level' Program: p: $p\n")
            //        }

            case TopLevel(e) =>
                println(s" p: $p \n\t TopLevel(e): $e\n")
                pB match {
                    case EmptyTopLevel =>
                        val (v, st) = evalExprCPS(EmptyExpression, 0, breakN, e,
                            EmptyEnvironment.asInstanceOf[LettuceEnvironment],
                            StoreInterface.emptyStore, (v: Value, s: LettuceStore) => (v, s))
                        v


                    case TopLevel(let@Let(_, _, _)) =>

                        val (v, st) = evalExprCPS(let, 0, -1, e,
                            EmptyEnvironment.asInstanceOf[LettuceEnvironment],
                            StoreInterface.emptyStore,
                            (v: Value, s: LettuceStore) => (v, s))
                        v


                    case TopLevel(funCall@FunCall(_, _)) =>
                        val (v, st) = evalExprCPS(funCall, 0, -1, e,
                            EmptyEnvironment.asInstanceOf[LettuceEnvironment],
                            StoreInterface.emptyStore,
                            (v: Value, s: LettuceStore) => (v, s))

                        v

                    /* Can only break at expressions: Let, LetRec, FunCall */
                    case m@_ => throw new IllegalArgumentException(s"Error: ($m) not a ' Break Top Level' Program: p: $p\n") // TOOD check if ths is still an sissues
                }


            case m@_ => throw new TypeConversionError(s"Error: $m not a 'Top Level' Program: p: $p\n")
        }
    }


//    def binOpIDK[T](eB: Expr, currN: Int, breakN: Int, e1: Expr, e2: Expr, env: LettuceEnvironment, st: LettuceStore )
//                (g: Value => T)
//                (f: (T,T) => Value): (Value, LettuceStore) = {
//        val (v1, st1) = evalExprCPS(eB,currN, breakN, e1, env, st, k)
//        val (v2, st2) = evalExprCPS(eB,currN, breakN, e2, env, st1, k)
//        ( f(g(v1), g(v2)), st2)
//    }
//
//    def unOpIDK[T](eB: Expr,currN: Int, breakN: Int, e: Expr, env: LettuceEnvironment, st: LettuceStore) (g: Value => T) (f: T => Value) : (Value, LettuceStore) = {
//        val (v, st1) = evalExprCPS(eB,currN, breakN, e, env, st)
//        (f(g(v)), st1)
//    }


//    eB: Expr =>Boolean
//    def evalExprCPS[T](eB: Expr, currN: Int, breakN: Int, e: Expr, env: LettuceEnvironment, st: LettuceStore): (Value, LettuceStore) = {

// (eB: currN, breakN) are used to determined when to break
/* (eB: currN, breakN) are used to determined when to break.
*
* - Can only break at ( currN will increase when one of the eval subexpression is called:
*   - Let
*   - LetRec
*   - FunDef
*   - FunCall
*  */

def evalExprCPS[V](eB: Expr, currN: Int, breakN: Int, e: Expr, env: LettuceEnvironment, st: LettuceStore, k: (Value, LettuceStore) => (Value, LettuceStore)): (Value, LettuceStore) = {

    def binOp(e1: Expr, e2: Expr)(fun: (Double, Double) => Double): (Value, LettuceStore) = {
        evalExprCPS(eB, currN, breakN, e1, env, st, (v1, st) => LettuceValue.valueToNumCPS(v1, st, (d1, st1) =>
            evalExprCPS(eB, currN, breakN, e2, env, st1, (v2, st1) => LettuceValue.valueToNumCPS(v2, st1, (d2, st1) => {
//                println(s"$v1, $v2\n")
                k(NumValue(fun(d1, d2)), st1)
            }
            ))))
    }


    def unOp(e1: Expr)(fun: Double => Double) = {
        evalExprCPS(eB, currN, breakN, e1, env, st, (u1, st1) => LettuceValue.valueToNumCPS(u1, st1, (v1, st1) =>
            k(NumValue(fun(v1)), st1)))
    }

    def compNumOp(e1: Expr, e2: Expr)(fun: (Double, Double) => Boolean) = {
        evalExprCPS(eB, currN, breakN, e1, env, st, (u1, st1) => LettuceValue.valueToNumCPS(u1, st1, (v1, st1) =>
            evalExprCPS(eB, currN, breakN, e2, env, st1, (u2, st2) => LettuceValue.valueToNumCPS(u2, st2, (v2, st2) =>
                k(BoolValue(fun(v1, v2)), st2)))))
    }


    def compBoolOp(e1: Expr, e2: Expr)(fun: (Boolean, Boolean) => Boolean) = {
        evalExprCPS(eB, currN, breakN, e1, env, st, (u1, st1) => LettuceValue.valueToBoolCPS(u1, st1, (v1, st1) =>
            evalExprCPS(eB, currN, breakN, e2, env, st1, (u2, st2) => LettuceValue.valueToBoolCPS(u2, st2, (v2, st2) =>
                k(BoolValue(fun(v1, v2)), st2)))))
    }

    def compUnOp(e1: Expr)(fun: Boolean => Boolean) = {
        evalExprCPS(eB, currN, breakN, e1, env, st, (u1, st1) => LettuceValue.valueToBoolCPS(u1, st1, (v1, st1) =>
            k(BoolValue(fun(v1)), st1)))
    }


    //println(s"currN: $currN, eB: $eB\n")

//    if(e match
//        e(BreakValue(eNew, env, st, currNew, breakNew), _ )) => valueToBreakCPS(eNew, currNew, breakNew, e, env, str, k))
//    else {
    e match {


//    eBNew: Expr, oldE: Expr, env: LettuceEnvironment, st: LettuceStore,
//    currN: Int, breakNew: Int, k: (Expr, LettuceStore) =>  (Value, LettuceStore))

//        case BreakValue(eNew, env, st, currNew, breakNew) => valueToBreakCPS(BreakValue(eNew, e, env, st, currNew,
        //                case e if matches(eB,e) =>
        //                    ???
        case ConstNum(f) =>
//            println(s"Cost: e: $e, f: $f\n")
            k(NumValue(f), st)

        case ConstBool(b) => k(BoolValue(b), st)
        case Ident(s) => k(env.lookup(s), st)

        case Plus(e1, e2) => binOp(e1, e2)(_ + _)
        case Minus(e1, e2) => binOp(e1, e2)(_ - _)
        case Mult(e1, e2) => binOp(e1, e2)(_ * _)
        case Div(e1, e2) => binOp(e1, e2) {
            case (_, 0.0) => throw new RuntimeError("Division by zero")
            case (v1, v2) => (v1 / v2)
        }

        case Log(e1) => unOp(e1) {
            case v if v > 0.0 => math.log(v)
            case v => throw new RuntimeError(s"Log of a negative number ${e} evaluates to ${v}!")
        }
        case Exp(e1) => unOp(e1)(math.exp)
        case Sine(e1) => unOp(e1)(math.sin)
        case Cosine(e1) => unOp(e1)(math.cos)

        case Geq(e1, e2) => compNumOp(e1, e2)(_ >= _)
        case Eq(e1, e2) => compNumOp(e1, e2)(_ == _)
        case Gt(e1, e2) => compNumOp(e1, e2)(_ > _)
        case Neq(e1, e2) => compNumOp(e1, e2)(_ != _)
        //
        case And(e1, e2) =>
            // TODO: Check if it "Short circuit" eval of And
            evalExprCPS(eB, currN, breakN, e1, env, st, (u1, st1) => LettuceValue.valueToBoolCPS(u1, st1, (v1, st1) =>
                evalExprCPS(eB, currN, breakN, e2, env, st1, (u2, st2) => LettuceValue.valueToBoolCPS(u2, st2, (v2, st2) =>
                    k(BoolValue(v1 && v2), st2)))))


        case Or(e1, e2) =>
            // TODO: Check if it "Short circuit" eval of Or
            evalExprCPS(eB, currN, breakN, e1, env, st, (u1, st1) => LettuceValue.valueToBoolCPS(u1, st1, (v1, st1) =>
                evalExprCPS(eB, currN, breakN, e2, env, st1, (u2, st2) => LettuceValue.valueToBoolCPS(u2, st2, (v2, st2) =>
                    k(BoolValue(v1 || v2), st2)))))


        case Not(e1) =>
            evalExprCPS(eB, currN, breakN, e1, env, st, (v1, st1) => LettuceValue.valueToBoolCPS(v1, st1, (b1, st1) =>
                k(BoolValue(!b1), st1)))

//        case IfThenElse(eC, e1, e2) =>
//            evalExprCPS(eB, currN, breakN, eC, env, st, (v1, st1) => LettuceValue.valueToBoolCPS(v1, st1,  (b1, st1) =>
//                b1 match {
//
//                    case BoolValue(true) => evalExprCPS(eB, currN, breakN, e1, env, st1, k)
//                    case BoolValue(false)=> evalExprCPS(eB, currN, breakN, e1, env, st1, k)
////                    case BoolValue(false),=> evalExprCPS(eB, currN, breakN, e2, env, st1, k)
//                    case _ => throw new RuntimeError(s"If-then-else condition expr: ${eC} is non-boolean")
//                }))

        case IfThenElse(eC, e1, e2 ) =>
            val k2 = (v: Value, st1: LettuceStore) => {
                LettuceValue.valueToBoolCPS(v, st1, (b, st1) =>
                    if(b)   evalExprCPS(eB, currN, breakN, e1, env, st, k)
                    else    evalExprCPS(eB, currN, breakN, e2, env, st, k))
            }
            evalExprCPS(eB, currN, breakN, eC, env, st, k2)
















        //

        //                case Block(eList) => // TODO: Is this needed?
        //                    if (eList.isEmpty) {
        //                        throw new SyntaxError("block of zero length is not allowed")
        //                    } else {
        //                        eList.foldLeft[(Value, LettuceStore)](NumValue(-1), st) { case ((_, st1), eHat) => evalExprCPS(eB, currN, breakN, eHat, env, st1) }
        //                    }


        case Let(x, e1, e2) =>
            eB match {
                case Let(eBs, eBe, _) if (x == eBs && e1 == eBe) =>
                    k(BreakValue(e, env, st, currN, breakN), st)

                case _ if (currN == breakN) => // DEBUG maybe this isnt good

                    k(BreakValue(e, env, st, currN, breakN), st)


                case _ =>

                    val k2 = (v: Value, s: LettuceStore) => {
                        val newEnv = EnvironmentUtils.make_extension(List(x), List(v), env) // TODO: Double check that "list" is OK for store
                        evalExprCPS(eB, currN + 1, breakN, e2, newEnv, s,
                            k)
                    }
                    evalExprCPS(eB, currN, breakN, e1, env, st, k2)
            }

        case LetRec(f, fd, e2) =>
            //  println(s"f: $f\nfd: $fd\ne2: $e2, currN: $currN, breakN: $breakN\n")
            fd match {
                case FunDef(xList, e1) =>
                    //                            println(s"FunDef(xList = $xList, e1 = $e1\ne2 =$e2)\n")
                    val newEnv = ExtendEnvRec(f, xList, e1, env)
                    evalExprCPS(eB, currN, breakN, e2, newEnv, st, k) // TODO: +c++?
            }

        //
        case FunDef(xList, e1) => k(Closure(xList, e1, env), st)
        //

//
//                            ---------------------------------
//
//                        case FunCallNOTCPS(fExpr, aList) =>
//                            val (v, st1) = evalExpr(newN, breakN, fExpr, env, st)
//
//                            val vc = LettuceValue.valueToClosure(v)
//                            val (vList, stAfterArgEval) = aList.foldLeft[(List[Value], LettuceStore)](List(), st1) {
//                                case ((vList1, st2), newExpr) =>
//                                    val (v, st3) = evalExpr(newN, breakN, newExpr, env, st2)
//                                    (vList1 ++ List(v), st3)
//
//                            }
//                            vc match {
//                                case Closure(fArgs, eHat, capturedEnv) =>
//                                    val newEnv = EnvironmentUtils.make_extension(fArgs, vList, capturedEnv)
//                                    evalExpr(newN, breakN, eHat, newEnv, stAfterArgEval)
//
//                                //case _ => throw new TypeConversionError("Converting from non closure to a closure value")
//                            }
//                            */

//            case class Closure(argList: List[String], e: Expr, sigma: LettuceEnvironment) extends Value
//                            -----------------------------------------------
//                        case FunCall(e1, e2) =>
//                            evalExprCPS(eB, currN, breakN, e1, env, st, (v1, st1) =>
//                            evalExprCPS(eB, currN, breakN, e2(0), env, st1, (v2, st2) =>
//                                LettuceValue.valueToClosureCPS(v1, st2, (c, st2) =>
//                                    c match {
//                                        case Closure(argList, closure_ex, closure_env) =>
//
//                                            val newEnv = EnvironmentUtils.make_extension(argList,e2, closure_env)
//                                            evalExprCPS(eB, currN, breakN, closure_ex, newEnv, st2, k)
//
//                                        case _ => throw new RuntimeError(s"Function Call error ($e1) does not eval to a Closure. (Also, this implementation covers functions with ONE argument)\n")
//                                    }
//                                )))

//











                case FunCall(e1, e2) =>
                    evalExprCPS(eB, currN, breakN, e1, env, st, (v1, st1) =>
                        evalExprCPS(eB, currN, breakN, e2(0), env, st1, (v2, st2) =>
                            v1 match {
                                case Closure(x, closure_ex, closed_env) =>
                                    val new_env =  EnvironmentUtils.make_extension(x, List(v2), closed_env)// closed_env + (x -> v2) //  EnvironmentUtils.make_extension(argList,e2, closure_env)
                                    evalExprCPS(eB, currN, breakN, closure_ex, new_env, st2, k)


                                case _ => throw new RuntimeError(s"Function Call error ($e1) does not eval to a Closure. (Also, this implementation covers functions with ONE argument)\n")
                            }))




        // TODOD: CPS FunCall
/*        case FunCall(fExpr, aList) =>
            val (v, st1) = evalExprCPS(eB, currN, breakN, fExpr, env, st, k)

            val (vc, _ ) = LettuceValue.valueToClosureCPS(v, st1, k) // TODOD CHECK which st1 i should use for the lcosure (St pr st1_
//            val (vList, stAfterArgEval) = aList.foldLeft[(List[Value], LettuceStore)](List(), str1) {
                val (vList, stAfterArgEval) = aList.foldLeft[(List[Value], LettuceStore)](List(), st1) { // OK : ST1 JHERE
                case ((vList1, st2), newExpr) =>
                    val (v, st3) = evalExprCPS(eB, currN + 1, breakN, newExpr, env, st2, k) // todo l DOUBEL CHECK THIS CURR++ OK
                    (vList1 ++ List(v), st3)

            }
            vc match {
                case Closure(fArgs, eHat, capturedEnv) =>
                    val newEnv = EnvironmentUtils.make_extension(fArgs, vList, capturedEnv)
                    evalExprCPS(eB, currN, breakN, eHat, newEnv, stAfterArgEval, k) // TODO: Make sure you increase currN CORRECTLY

//                case m @ _ =>
//                    throw new TypeConversionError(s"Converting from non closure ($m) to a closure value (from eval of FunCall)") // Srirm removed


            }*/










        /*  case FunCall(fExpr, aList) =>
//                    println(s"FunCall(fExpr:  $fExpr\naList: $aList\n currN: $currN, breakN: $breakN\n")
//
//                    if (currN == breakN) {
//                        println("here?")
//
//                        (BreakValue(currN, e, env, st), st)
//
//                    } else {
                        eB match {
                            case FunCall(eBCall, listB) if ( fExpr ==  eBCall && aList == listB) => // T
                                k(BreakValue( e, env, st, currN, breakN), st)

                            case FunCall(_, _) if (currN == breakN ) => {
                                k(BreakValue( e, env, st, currN, breakN), st)
                            }

                            case _ => { // T
//                                ???

//                                case _ =>  { // Org
                                    val (v, st1) = evalExprCPS(eB, currN, breakN, fExpr, env, st,k)
                                    val vc = LettuceValue.valueToClosureCPS(v, st, k)
                                    val (vList, stAfterArgEval) = aList.foldLeft[(List[Value], LettuceStore)](List(), st1) {
                                        case ((vList1, st2), newExpr) =>
                                            val (v, st3) = evalExprCPS(eB, currN + 1, breakN, newExpr, env, st2,k)
                                            (vList1 ++ List(v), st3)



                                    }
                                    vc match {
                                        case (Closure(fArgs, eHat, capturedEnv), sC) =>
                                            val newEnv = EnvironmentUtils.make_extension(fArgs, vList, capturedEnv)
                                            evalExprCPS(eB, currN+1, breakN, eHat, newEnv, stAfterArgEval,k)

                                        case _ => throw new TypeConversionError("Converting from non closure to a closure value")
                                    }
                                }
*/


        //
        //                                evalExprCPS(eB, currN, breakN, fExpr, env, str, (v, st1) => (vList, stAfterArgEval) =>
        //                                    aList.foldLeft[(List[Value], LettuceStore)](List(), st1) {
        //                                        case ((vList1, st2), newExpr) =>
        //                                            evalExprCPS(eB, currN + 1, breakN, newExpr, env, st2, (v, st3) => (vList ++ List(v), st3))
        //                                    }

        //                            }

        //   re write
        //                                val (v, st1) = evalExprCPS(eB, currN, breakN, fExpr, env, st)
        //                                val (vList, stAfterArgEval) = aList.foldLeft[(List[Value], LettuceStore)](List(), st1) {
        //                                    case ((vList1, st2), newExpr) =>
        //                                        val (v, st3) = evalExprCPS(eB, currN + 1, breakN, newExpr, env, st2)// should i move the +1?
        //                                        (vList ++ List(v), st3)
        //                                 }
        //                                val vc = LettuceValue.valueToClosure(v)
        //                                vc match {
        //                                    case Closure(fArgs, eHat, capturedEnv) =>
        //                                        val newEnv = EnvironmentUtils.make_extension(fArgs, vList, capturedEnv)
        //                                        evalExprCPS(eB, currN+1, breakN, eHat, newEnv, stAfterArgEval)
        //                                    case _ => throw new TypeConversionError("Converting from non closure to a closure value")
        //
        //                                }
        //
        //
        //                            case _ =>  { // Org
        //                                val (v, st1) = evalExprCPS(eB, currN, breakN, fExpr, env, st)
        //                                val vc = LettuceValue.valueToClosure(v)
        //                                val (vList, stAfterArgEval) = aList.foldLeft[(List[Value], LettuceStore)](List(), st1) {
        //                                    case ((vList1, st2), newExpr) =>
        //                                        val (v, st3) = evalExprCPS(eB, currN + 1, breakN, newExpr, env, st2)
        //                                        (vList1 ++ List(v), st3)
        //
        //
        //
        //                                }
        //                                vc match {
        //                                    case Closure(fArgs, eHat, capturedEnv) =>
        //                                        val newEnv = EnvironmentUtils.make_extension(fArgs, vList, capturedEnv)
        //                                        evalExprCPS(eB, currN+1, breakN, eHat, newEnv, stAfterArgEval)
        //
        //                                    //case _ => throw new TypeConversionError("Converting from non closure to a closure value")
        //                                }
        //                            }


        //                        }
        //
        //                    }
        case NewRef(e1) =>
            val k2 = (v: Value, st1: LettuceStore) => {
                val (j, st2) = StoreInterface.mkNewReference(st1, v)
                k(Reference(j), st2) // TODO: check if its st2

            }
            evalExprCPS(eB, currN, breakN, e1, env, st, k2)

        //
        //                  // Org
        //                case NewRef(e1) =>
        //                    val (v, st1) = evalExprCPS(eB,currN, breakN, e1, env, st)
        //                    val (j, st2) = StoreInterface.mkNewReference(st1, v)
        //                    (Reference(j), st2)


        case DeRef(e1) =>
            val k2 = (r: Value, st1: LettuceStore) => {
                r match {
                    case Reference(j) =>
                        k(LettuceValue.valueToReferenceCPS(r, st1, (j, st1) => r), st1)
//                    case m @ _ => throw new TypeConversionError(s"Converting from non closure ($m) to a closure value (from eval of DeRef)")

                }

            }
            evalExprCPS(eB, currN, breakN, e1, env, st, k2)



        //                case DeRef(e1) => {
        ////
        //                    evalExprCPS(eB, currN, breakN, e1, env, st, (v, st1) => {
        //                        StoreInterface.mkDeref(j, st1) => LettuceValue.valueToReferenceCPS(v, st, (j, st1)  => k(v, st1)
        //                    })}


        //                  // Origin
        //                case DeRef(e1) =>
        //                    val (v, st1) = evalExpr(e1, env, st)
        //                    val j = LettuceValue.valueToReference(v)
        //                    (StoreInterface.mkDeref(j, st1), st1)


        //                case AssignRef(e1, e2) => {
        //                    val (v1, st1) = evalExprCPS(eB,currN, breakN, e1, env, st, k)
        //                    v1 match {
        //                        case Reference(j) => {
        //                            val (v2, st2) = evalExprCPS(eB, currN, breakN, e2, env, st1, k)
        //                            val st3 = StoreInterface.mkAssign(j, v2, st2)
        //                            k(v2, st3)
        //                        }

        //                        case NumValue(d) => {
        //                            k(v1, st1)
        //
        //                        }

        //                        case m @ _ => {
        //                                println(s"AssignRef -> case _ == $m\n")
        //                                throw new TypeConversionError(s"Can't convert from $m to Reference(j) (for AssignRef)")
        //                            }
        //                        }
        //                    }
        //                 // Org
        //                case AssignRef(e1, e2) =>
        //                    val (v1, st1) = evalExprCPS(eB,currN, breakN, e1, env, st)
        //                    val j = LettuceValue.valueToReference(v1)
        //                    val (v2, st2) = evalExprCPS(eB,currN, breakN, e2, env, st1)
        //                    val st3 = StoreInterface.mkAssign(j, v2, st2)
        //                    (v2, st3)


        case AssignRef(e1, e2) =>
            val (r, st1) = evalExprCPS(eB, currN, breakN, e1, env, st, k)
            r match {

                case Reference(j) =>
                    //                                val jj = LettuceValue.valueToReference(r, st1,  (j, st1))

                    val (v2, st2) = evalExprCPS(eB, currN, breakN, e2, env, st1, k)
                    val st3 = StoreInterface.mkAssign(j, v2, st2)
                    k(v2, st3)

                case  m @ _ =>
                    println(s"Assign Error: m: $m  (${m.getClass}) \n, If, the assignemtn is 'Identifier(assign), please rewrite your program with 'assignref x <- y' (NOT: 'assignref(x , y)') \n")
                    //            k(m, st)
                    throw new RuntimeError(s"AssignRef Error: m: $m (not a Reference),\n")

            }



    }}}





