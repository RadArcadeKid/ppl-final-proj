package LettuceAST

object LettuceConsole {
    var debug = true
    var stepMode = false
    var breakN = -1 //start at -1 so the program can step to 0
    var retStr = "" //the string to pass in to the lettuce expression
    var quitting = false

    def readOneProgram(): (Boolean, String, Int) = {
        var debugChoice = "" //determines which choice to debug
        var debugCurrent = false //user is not currentlyDebugging
        //var breakN = -1 //start at -1 so the program can step to 0

        if(!stepMode){ //if we're not entering stepMode, then proceed as normal
            quitting = false
            retStr = "" //resetting every time
            breakN = -1 //reset breakN too

            var s: String = scala.io.StdIn.readLine()
            if (s == "exit;;"){
                sys.exit(0) //leave
                //return (false, "", -1)
            }

            while (!s.endsWith(";;")){
                retStr = retStr + s
                s = scala.io.StdIn.readLine("|")
            }

            retStr = retStr + s.dropRight(2) + "\n"
        }


        //debugging mode here
        if (debug){
            debugCurrent = true;
            println(retStr)

            while(debugCurrent){
              //println("  ** DEBUG MODE **")
              println("--Would you like to:\n   [L] - Enter a specific step/line number of the program?")
              if(breakN == -1){
                print(s"   [S] - Step ahead starting at 0?")
              }
              else{
                val breakNnext = breakN + 1;
                print(s"   [S] - Step ahead from $breakN -> $breakNnext ?")
              }
              print("\n   [Q] - Quit debugging this program?")

              print("\n   (Enter L, S, or Q): ")
              debugChoice = scala.io.StdIn.readLine()

              debugChoice match {
                  case "S" | "s" => { //step thru to next value
                      breakN = breakN + 1
                      debugCurrent = false
                      quitting = false
                      stepMode = true
                  }
                  case "L" | "l" => { //examine line number at the desired value:
                      print("\n  Enter non-negative int, Step n = ")
                      breakN = scala.io.StdIn.readInt()
                      stepMode = true
                      debugCurrent = false
                      quitting = false
                  }
                  case "Q" | "q" => {
                      println(" -- Quitting debug mode!")
                      stepMode = false
                      debugCurrent = false
                      quitting = true
                  }
                  case _ => {
                      println(s"\n Error: Not a valid option. Make sure to enter capital letters! \n")
                  }
              }
            }
            if(!quitting){
              println(s" -- STEPPING TO n = $breakN")
            }

            println("--------------------------")
        }
        debugChoice = "" //reset after exiting
        return (true, retStr, breakN)
    }



    def processInput(s: String, n: Int): Value = {
          val p: Program = new LettuceParser().parseString(s)
          if (debug && !quitting) {
                println(s"-- Step: $n")
                println("-- Top Level Expression: ")
                println(s"        $p \n")
          }

          val v = LettuceInterpreter.evalProgram(p, n)

          if(!quitting){
            outputReturnValue(v, n);
          }

          v
    }


    def outputReturnValue(v: Value, n:Int): Value = v match {
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
            print(s" -- Choose what to view for step $n: --  \n")
            print("[0] = Back Out \n[1] = Expr \n[2] = Environment \n[3] = Store\n Option: ")

            //val viewBreak: Int = scala.io.StdIn.readInt()
            val viewBreak = scala.io.StdIn.readLine() //changed to any for compatibility


            print("\n")

            viewBreak match {
                case "0" => {
                    breakDebug = false;
                }
                case "1" => {
                    println(s"  Expr: $e")
                }
                case "2" => {
                    println(s"  Environment: $env")

                }
                case "3" => {
                    println(s"  Environment: $st")

                }
                case _ => {
                    println(s" Error: Not a valid option: $viewBreak")
                    println("  (be sure to enter a valid number with no spaces)")
                }
            }
            if(viewBreak != "0") {
              print("\n -- press enter to continue -- ")
              scala.io.StdIn.readLine() //just makes the user hit enter when they're done
            }
            print("\n")
        }
    }

    def main(args: Array[String]): Unit = {
        println("\n \n \n")
        println("-------------------------------------------------------------")
        println("    *** Welcome to the Lettuce Breakpoint Debugger ***    ")
        println("       * A project by Chi Huynh and Jake Henson *   \n \n")
        print("    Enter a Lettuce program to start\n")
        print("    then ;; when you are finished entering. \n \n")
        print("    (You can exit the program by typing   exit;; \n")
        print("    when not in debug mode) \n ")
        print("------------------------------------------------------------- \n \n")
        while (true){
            print("\n -- Lettuce Program:\n|")
            try {
                val (b, s, n) = readOneProgram()
                if (b) {
                      val v = processInput(s, n)
                      println(s"v = $v")
                } else {
                    sys.exit(1)
                }
            } catch {
                case UnboundIdentifierError(msg) => println(s"Error: Unbound Identifier - $msg")
                case TypeConversionError(msg)=> println(s"Error: Type Conversion error - $msg")
                case SyntaxError(msg) => println(s"Syntax Error: $msg")
                case RuntimeError(msg) => println(s"Runtime Error: $msg")
            }
        }
    }


}
