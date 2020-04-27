package LettuceAST

object LettuceConsole {
    val debug = true
    /* - read in from the standard input and parse/interpret -*/
    def readOneProgram(): (Boolean, String, Int) = {
        var retStr = ""
        var breakN = 0;
        var s: String = scala.io.StdIn.readLine()
        if (s == "exit;;" ){
            return (false, "", -1)
        }
        while (!s.endsWith(";;")){
            retStr = retStr + s
            s = scala.io.StdIn.readLine("|")
        }
        retStr = retStr + s.dropRight(2) + "\n"
        if (debug){
            //println(" --- Debug --- ")
            println(retStr)
            print("Enter non-negative int, n = ")
            breakN = scala.io.StdIn.readInt()
            //println(" --- Debug --- ")
        }
        return (true, retStr, breakN)
    }

    def processInput(s: String, n: Int): Value = {
        val p: Program = new LettuceParser().parseString(s)
        if (debug) {
            //println("--- Debug --- ")
            println(p)
            //println("--- Debug ---")
        }
        val v = LettuceInterpreter.evalProgram(p, n)
        outputReturnValue(v);
        v
    }

    def outputReturnValue(v: Value): Value = v match { // TODO: what return type should it have?
        case BreakValue(eB, envB, stB) => {

            println(s"Returned Break Value:\n\tExpr: $eB\n" )
            returnBreakValueOptions(v, eB, envB, stB)
            v

        }
        case _ =>{
            println(s"Returned value : $v")
            v

        }
    }


    def returnBreakValueOptions(v: Value, e: Expr, env: LettuceEnvironment, st: LettuceStore): Unit =  {
        var breakDebug = true;
        while(breakDebug){
            print("\n--------------------------\n:Viewing Options:\n0 = Exit \n1 = Expr \n2 = Environment \n3 = Store\nOption: ")
            val viewBreak: Int = scala.io.StdIn.readInt()

            viewBreak match {
                case 0 => {
                    breakDebug = false;

                }
                case 1 => {
                    println(s"Expr: $e")

                }
                case 2 => {
                    println(s"Environment: $env")

                }
                case 3 => {
                    println(s"Environment: $st")

                }
                case _ => {
                    println(s"Error: Not a valid option: $viewBreak\n")
                }

            }

        }





    }
    def main(args: Array[String]): Unit = {
        while (true){
            print("> ")
            try {
                val (b, s, n) = readOneProgram()
                if (b) {
                    val v = processInput(s, n)
                } else {
                    sys.exit(1)
                }
            } catch {
                case UnboundIdentifierError(msg) => println(s"Error: Unbound Identifier - $msg")
                case TypeConversionError(msg)=> println(s"Error: Type Conversion error $msg")
                case SyntaxError(msg) => println(s"Syntax Error: $msg")
                case RuntimeError(msg) => println(s"Runtime Error: $msg")
            }
        }
    }


}
