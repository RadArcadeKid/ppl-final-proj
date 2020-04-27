package LettuceAST

object LettuceConsole {
    val debug = true
    var breakN = -1 //start at -1 so the program can step to 0

    /* - read in from the standard input and parse/interpret -*/
    def readOneProgram(): (Boolean, String, Int) = {
        var retStr = "" //retStr is odd... maybe change this to go back to debug mode?
        var debugChoice = ""
        var debugCurrent = false;

        //var breakN = 0
        var s: String = scala.io.StdIn.readLine()
        if (s == "exit;;" ){
            return (false, "", -1)
        }

        //TODO: CHANGE THIS LINE SO IT DOESN'T CHANGE THE PROGRAM WHEN STEPPING
        while (!s.endsWith(";;")){
            retStr = retStr + s
            s = scala.io.StdIn.readLine("|")
        }

        retStr = retStr + s.dropRight(2) + "\n"

        //debugging mode here
        if (debug){

            debugCurrent = true;
            println(retStr)

            while(debugCurrent){
              println("  Would you like to enter a specific line number of the program? [L]")
              if(breakN == -1){
                print(s"  Or step ahead from line 0? [S]")
              }
              else{
                print(s"  Or step ahead from line $breakN ? [S]")
              }
              print("\n   (Enter S or L): ")
              debugChoice = scala.io.StdIn.readLine()
              debugChoice match {
                  case "S" => { //step thru to next value
                      breakN = breakN + 1
                      debugCurrent = false
                  }
                  case "L" => { //examine line number at the desired value:
                      print("\n  Enter non-negative int, Step n = ")
                      breakN = scala.io.StdIn.readInt()
                      debugCurrent = false
                  }
                  case _ => {
                      println(s"\n Error: Not a valid option. Make sure to enter capital letters! \n")
                  }
              }
            }
            println(s" -- STEPPING TO n = $breakN")

            // print("Enter non-negative int, n = ")
            // breakN = scala.io.StdIn.readInt()
            println("--------------------------")
        }
        return (true, retStr, breakN)
    }

    def processInput(s: String, n: Int): Value = {
        val p: Program = new LettuceParser().parseString(s)
        if (debug) {
            println(s"-- Line: $n")
            println("-- Top Level Expression: ")
            println(s"        $p \n")
        }

        val v = LettuceInterpreter.evalProgram(p, n)
        outputReturnValue(v, n);  //n is now here to get line data
        v
    }


    def outputReturnValue(v: Value, n:Int): Value = v match { // TODO: what return type should it have?
        case BreakValue(eB, envB, stB) => {

            println(s"-- Returned break value:\n\tExpr: $eB\n" )
            returnBreakValueOptions(v, eB, envB, stB, n)
            v

        }
        case _ =>{
            println(s"-- Returned value: \n  $v")
            v

        }
    }


    def returnBreakValueOptions(v: Value, e: Expr, env: LettuceEnvironment, st: LettuceStore, n: Int): Unit =  {
        var breakDebug = true;
        while(breakDebug){
            println("--------------------------")
            print(s" -- Choose what to view for line $n: --  \n")
            print("[0] = Exit \n[1] = Expr \n[2] = Environment \n[3] = Store\n Option: ")

            val viewBreak: Int = scala.io.StdIn.readInt()
            print("\n")

            viewBreak match {
                case 0 => {
                    breakDebug = false;

                }
                case 1 => {
                    println(s"  Expr: $e")
                }
                case 2 => {
                    println(s"  Environment: $env")

                }
                case 3 => {
                    println(s"  Environment: $st")

                }
                case _ => {
                    println(s" Error: Not a valid option: $viewBreak\n")
                }
            }
            print("\n (Press enter to continue) ")
            scala.io.StdIn.readLine() //just makes the user hit enter when they're done
            print("\n")
        }

    }
    def main(args: Array[String]): Unit = {
        println("\n \n \n")
        println("-------------------------------------------------")
        println("    *** Welcome to the Lettuce Debugger ***    ")
        println("   * A project by Chi Huynh and Jake Henson *   \n \n")
        print("    Enter a Lettuce program to start\n")
        print("    then ;; when you are finished entering. \n \n")
        print("    (You can exit the program by typing   exit;; \n")
        print("    when not in debug mode) \n ")
        print("------------------------------------------------- \n")
        while (true){
            print("| \n|")
            try {
                val (b, s, n) = readOneProgram()
                if (b) {
                    val v = processInput(s, n)
                } else {
                    sys.exit(1)
                }
            } catch { //TODO: Handle catching better - there's an error reading input sometimes
                case UnboundIdentifierError(msg) => println(s"Error: Unbound Identifier - $msg")
                case TypeConversionError(msg)=> println(s"Error: Type Conversion error $msg")
                case SyntaxError(msg) => println(s"Syntax Error: $msg")
                case RuntimeError(msg) => println(s"Runtime Error: $msg")
            }
        }
    }


}
