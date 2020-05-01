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
            if (s == "exit;;" || s == "quit;;"){
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
              println("--Would you like to:\n   [L] - Enter a specific step/expression of the program?")
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
                    print("\n -- Enter 'e' to break at an expression, \n -- Enter 'n' to break at a line number: \n     ")

                    val lString = scala.io.StdIn.readLine()

                    lString match {
                      case "n" | "N" => {

                        print("\n  Enter non-negative int, Step n = ")
                        //val callExp = processINput()
                        breakN = scala.io.StdIn.readInt()
                        stepMode = true
                        debugCurrent = false
                        quitting = false
                      }

                      case "e" | "E" => {
                        breakN = -1
                        stepMode = true // keep as true
                        debugCurrent = false
                        quitting = false
                      }

                      case _ => {
                        println(s"\n Error: Not a valid option. Inside of L! \n")
                      }
                    }
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
            if(!quitting && breakN != -1){
              println(s" -- STEPPING TO n = $breakN")
            }

            println("--------------------------")
        }

        //for skipping the above loop
        if(!debug){
          debug = true; //this is so that if the inital menu gets skipped it'll come back to it
        }
        debugChoice = "" //reset after exiting
        return (true, retStr, breakN)
    }


    def processInput(s: String, n: Int): Value = {
        val p: Program = new LettuceParser().parseString(s)


        if(n == -1 && !quitting){ ///if the user wants to enter a specific let expression
            var inputCorrect = false //set up a bool here for checking
            var breakStringUserInput = "" //emptyString
            while(!inputCorrect){
              print(" -- Enter let expression you want to break at = (Ex: let x = 2 in _ ): ")
              breakStringUserInput = scala.io.StdIn.readLine()
              inputCorrect = checkInput(breakStringUserInput) //make sure the user enters a valid string here!
            }

            val pB: Program = new LettuceParser().parseString(breakStringUserInput) //parse the string

            if (debug && !quitting) {
              println(s"-- Step: $n")
              println("-- Top Level Expression: ")
              println(s"        $p \n")
            }

            val v = LettuceInterpreter.evalProgram(p, n, pB)

            if (!quitting) {
            outputReturnValue(v, n);
            }
            v

          }
          else {
            if (debug && !quitting) {
              println(s"-- Step: $n")
              println("-- Top Level Expression: ")
              println(s"        $p \n")
            }

            val v = LettuceInterpreter.evalProgram(p, n, EmptyTopLevel)

            if (!quitting) {
             outputReturnValue(v, n);

            }
            v
          }
          //v
    }

    //checks the bS (break input) input
    def checkInput(input: String): Boolean = {
        val startsWithLet = input.contains("let")
        val containsIn = input.contains("in")
        val containsEquals = input.contains("=")
        if(!startsWithLet){
            println(s"\n    Error - Your input was incorrect - the string $input does not start with a `let`")
            return false
        }
        if(!containsIn){
            println(s"\n    Error - Your input was incorrect - the string $input does not contain an `in`")
            return false
        }
        if(!containsEquals){
            println(s"\n    Error - Your input was incorrect - the string $input does not contain a `=` symbol")
            return false
        }
        return true
    }





    def outputReturnValue(v: Value, n:Int): Unit = v match {

        case BreakValue( e, env, st, currN, bN) => {
            if (breakN == -1) {
              breakN = currN
            }


            println(s"-- Returned From Break Value:\n\t(v = $v):\n\tExpr: $e\n" )
            returnBreakValueOptions(v, e, env, st, breakN)


        }
        case _ =>{
          println("Reached the end of current program! \n ")
          println(s"-- End of Program Output:\n\t $v ")
          stepMode = false

          print("\n \n (press enter to exit and run a new program) ")
          scala.io.StdIn.readLine() //just makes the user hit enter when they're done


          println("-------------------------------------------------------------------------------------------- \n")
        }
    }



    def returnBreakValueOptions(v: Value, e: Expr, env: LettuceEnvironment, st: LettuceStore, n: Int): Unit =  {
        var breakDebug = true;
        while(breakDebug){

            println("--------------------------")
            print(s" -- Choose what to view for step $n: --  \n")
            println("     [0] = Back Out ")
            println("     [1] = Expr")
            println("     [2] = Environment")
            println("     [3] = Store")
            val breakN_next = breakN + 1;
            println(s"     [4] = Step ahead to step $breakN_next")
            print("      Option: ")

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
                case "4" => { //TODO: a step forward here!
                    breakN = breakN + 1
                    breakDebug = false;
                    debug = false; //just to skip the second menu
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
            if(!stepMode){
              print("\n -- Enter NEW Lettuce Program (or exit;; to quit):\n > ")
            } else {
              print("\n -- Lettuce Program:\n| ")
            }

            try {
              val (b, s, n) = readOneProgram()

              b match {
                case true => {

                  val v = processInput(s, n)

                }
                case false => {
                  println ("Something went critically wrong!")
                  sys.exit (1)
                }
              }

            } catch {
                //TODO:  a case where it reaches the end without "erroring" out
                case UnboundIdentifierError(msg) => {
                  stepMode = false
                  println(s"Error: Unbound Identifier - $msg")
                }
                case TypeConversionError(msg)=> {
                  println(s"Error: Type Conversion error - $msg")
                  stepMode = false
                }
                case SyntaxError(msg) => {
                  stepMode = false
                  println(s"Syntax Error: $msg")
                }
                case RuntimeError(msg) => {
                  stepMode = false
                  println(s"Runtime Error: $msg")
                }
            }
        }

    }


}
