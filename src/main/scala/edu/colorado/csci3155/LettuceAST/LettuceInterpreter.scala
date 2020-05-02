package LettuceAST

import LettuceAST.LettuceInterpreter.evalExprCPS


case object LettuceInterpreter {
    def evalProgram(p: Program, breakN: Int, pB: Program): Value = {

        p match {

            case TopLevel(e) =>


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

                }

        }
    }


    /* CPS implementation of evalExpr using Lettuce */
    def evalExprCPS[V](eB: Expr, currN: Int, breakN: Int,
                       e: Expr, env: LettuceEnvironment, st: LettuceStore,
                       k: (Value, LettuceStore) => (Value, LettuceStore)): (Value, LettuceStore) = {

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


        e match {


            case ConstNum(f) => k(NumValue(f), st)

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
            case And(e1, e2) => {
                // TODO: Check if it "Short circuit" eval of And
                evalExprCPS(eB, currN, breakN, e1, env, st, (u1, st1) => LettuceValue.valueToBoolCPS(u1, st1, (v1, st1) =>
                    evalExprCPS(eB, currN, breakN, e2, env, st1, (u2, st2) => LettuceValue.valueToBoolCPS(u2, st2, (v2, st2) =>
                        k(BoolValue(v1 && v2), st2)))))
            }

            case Or(e1, e2) => {
                // TODO: Check if it "Short circuit" eval of Or
                evalExprCPS(eB, currN, breakN, e1, env, st, (u1, st1) => LettuceValue.valueToBoolCPS(u1, st1, (v1, st1) =>
                    evalExprCPS(eB, currN, breakN, e2, env, st1, (u2, st2) => LettuceValue.valueToBoolCPS(u2, st2, (v2, st2) =>
                        k(BoolValue(v1 || v2), st2)))))
            }

            case Not(e1) => {
                evalExprCPS(eB, currN, breakN, e1, env, st, (v1, st1) => LettuceValue.valueToBoolCPS(v1, st1, (b1, st1) =>
                    k(BoolValue(!b1), st1)))
            }

            case IfThenElse(eC, e1, e2) => {
                val k2 = (v: Value, st1: LettuceStore) => {
                    LettuceValue.valueToBoolCPS(v, st1, (b, st1) =>
                        if (b) evalExprCPS(eB, currN, breakN, e1, env, st, k)
                        else evalExprCPS(eB, currN, breakN, e2, env, st, k))
                }
                evalExprCPS(eB, currN, breakN, eC, env, st, k2)
            }

            //
            //        Not Using "Block"
            //        case Block(eList) =>
            //            if (eList.isEmpty) {
            //                throw new SyntaxError("block of zero length is not allowed")
            //            } else {
            //                eList.foldLeft[(Value, LettuceStore)](NumValue(-1), st) { case ((_, st1), eHat) => evalExprCPS(eB, currN, breakN, eHat, env, st1) }
            //            }


            case Let(x, e1, e2) => {
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
            }


            case LetRec(f, fd, e2) => {
                //  println(s"f: $f\nfd: $fd\ne2: $e2, currN: $currN, breakN: $breakN\n")
                fd match {
                    case FunDef(xList, e1) =>
                        // println(s"FunDef(xList = $xList, e1 = $e1\ne2 =$e2)\n")
                        val newEnv = ExtendEnvRec(f, xList, e1, env)
                        evalExprCPS(eB, currN + 1, breakN, e2, newEnv, st, k)
                }

            }


            case FunDef(xList, e1) => k(Closure(xList, e1, env), st)


            case FunCall(e1, e2) => {
                evalExprCPS(eB, currN, breakN, e1, env, st, (v1, st1) =>
                    evalExprCPS(eB, currN + 1, breakN, e2(0), env, st1, (v2, st2) =>
                        v1 match {
                            case Closure(x, closure_ex, closed_env) =>
                                val new_env = EnvironmentUtils.make_extension(x, List(v2), closed_env) // closed_env + (x -> v2) //  EnvironmentUtils.make_extension(argList,e2, closure_env)
                                evalExprCPS(eB, currN, breakN, closure_ex, new_env, st2, k)


                            case _ => throw new RuntimeError(s"Function Call error ($e1) does not eval to a Closure. (Also, this implementation covers functions with ONE argument)\n")
                        }))
            }

            case NewRef(e1) => {
                val k2 = (v: Value, st1: LettuceStore) => {
                    val (j, st2) = StoreInterface.mkNewReference(st1, v)
                    k(Reference(j), st2) // TODO: check if its st2

                }
                evalExprCPS(eB, currN, breakN, e1, env, st, k2)
            }


            case DeRef(e1) => {
                val k2 = (r: Value, st1: LettuceStore) => {
                    r match {
                        case Reference(j) =>
//                            println(s"e1: $e1, r: $r ")
                            LettuceValue.valueToReferenceCPS(r, st1, (j, st1) => k(st.m(j), st1))
                        //                    case m @ _ => throw new TypeConversionError(s"Converting from non closure ($m) to a closure value (from eval of DeRef)")
                    }

                }
                evalExprCPS(eB, currN, breakN, e1, env, st, k2)
            }


            case AssignRef(e1, e2) => {

                evalExprCPS(eB, currN, breakN, e1, env, st, (v1, st1) =>
                    v1 match {
                        case Reference(j) => evalExprCPS(eB, currN, breakN, e2, env, st1, (v2, st2) => {
                        val st3 = StoreInterface.mkAssign(j, v2, st2)
                                k(v2, st3)
                        })
                        case m@_ =>
                              println(s"Assign Error: m: $m  (${m.getClass}) \n, If, the assignemtn is 'Identifier(assign), please rewrite your program with 'assignref x <- y' (NOT: 'assignref(x , y)') \n")
                        throw new RuntimeError(s"AssignRef Error: m: $m (not a Reference),\n")
                    })

            }

        }
    }
}
