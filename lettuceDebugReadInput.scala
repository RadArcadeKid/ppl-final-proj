// Note: 
// - To run in the terminal use:
//    scala <nameOFfile.scala>
//    scala lettuceDebugReadInput.scala

import scala.io.StdIn

object LettuceDebugInput {
  
  def main(args: Array[String]): Unit = {
    class LettuceString(var s: String)
  
    // To avoid getting: "NumberFormatException" error if ["String".toInt] occurs
    // Code provided by: 
    // - https://alvinalexander.com/scala/how-cast-string-to-int-in-scala-string-int-conversion/
    def toIntF(s: String): Option[Int] = {
      try {       
       Some(s.toInt)
      } catch {
        case e: NumberFormatException => None
      }
    }
    
    def getNString(): String = {
      println("Which sub-eval number (positive integers only) do you want to break on?")
      print("[Positive Integer] n: ")
      StdIn.readLine()
    }

    def getLettuceExpr(): String  = {
      
          print(s"Enter Lettuce Expression to eval: ");
          val lettuce = StdIn.readLine()
 
          println(s"\nTODO: Use class's parser (w/: LetVar, AssignVar, etc. for ImmutableStore)")
          println(s"----------------------\nEval:\n  $lettuce")
          lettuce
    }


    def recN(nString: String, ls: LettuceString): Int = {
    
      val nTest = toIntF(nString) 
      
      if(nTest.getClass == (toIntF("0")).getClass) {
        
        var n = nString.toInt
        println(s"n: \n  $n")
        if(n > 0) {
          
          recN((n-1).toString, ls)         
        } else if (n == 0) {
          print("\n\nContinue to debug? Y/N: ")
          val ans = StdIn.readLine()
          if(ans == "y" || ans == "Y") {
            val newNString = getNString()
            println(s"Eval (Continue):\n  ${ls.s}")
            recN(newNString, ls)              
          } else {
            println("Exiting Debugger")
            0 
          }
                     
        }  else {
          println(s"\t*** Error: n = '$nString' is not a positive integer")
          0
      }
    } else {
      println(s"\t*** Error: n = '$nString' is not an int ")
      -999
      }
    } 
    val ls: LettuceString = new LettuceString("")
    val nString = getNString()
    ls.s = getLettuceExpr()
    recN(nString, ls)
  }
}

