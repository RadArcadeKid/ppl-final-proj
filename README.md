
# LETTUCE BREAKPOINT DEBUGGER

## Final project for CSCI 3155 by Chi Huynh and Jake Henson


### About:
This project is designed as a lettuce breakpoint analyzer, with inspirations from programs like GDB, except being a little bit more friendly.

Breakpoints are set by the “sub-eval expressions” (let statements), and can be traversed in several ways, detailed below. This will allow the user to look at their lettuce programs more in-depth and potentially understand how they are working, as well as gain a more in-depth insight into the Lettuce language.
_


### How to install/run

First, either clone this repo or download the project.

**This can be run either using the terminal with SBT or IntelliJ**

 - To install scala build tools
   https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Linux.html

 **To compile + run in terminal:**
  - To compile simply type

		   `sbt compile`


   - To run simply type

			`sbt run`

on your command line prompt.

This will run the "console" application for Lettuce.
  - If you get a prompt:
~~~
 Multiple main classes detected, select one to run:

 [1] LettuceAST.LettuceConsole
 [2] LettuceDebugInput
~~~
- Simply type `1` to continue....



### How to use/how it works:

This program basically acts as a kind of "shell" for running lettuce programs. Once you give it a Lettuce program, it will be run in steps and you can examine it in great detail.

Once the program has been run, below the introduction, you will see the following prompt
~~~
 -- Enter NEW Lettuce Program (or exit;; to quit):
 >
~~~
Now type your program on the console. (You can do this on one line or typed out with tabs).  Use `;;`  to end the program.

You can type a program like so:
~~~
> let x = 20 in
   let f = function(x) x - 45 in
    f(x) ;;
~~~

- (Remember to end your program with **two** semicolons when you're done!!)

You should see the output

~~~
--Would you like to:
   [L] - Enter a specific step/line number of the program?
   [S] - Step ahead starting at 0?
   [Q] - Quit debugging this program?
   (Enter L, S, or Q):

~~~

This is the "debug menu". From here, there are several options:

 - Typing `L` will let you enter a specific expression or line number
	 - This will open the sub-menu:
		 **- From here, you can break at either a line/step number or a let expression.**
		 - If you want to stop at a "let" expression, it must be input as as "let y = 2 in _"
			 - **Note** - To break at "let rec" they actually have to call the function with the argue value the you want , i.e. `letrec f = function(x) in .....`
you have to type the actual value, i.e., `f(3)`

- Typing `S` will give you the option to step forward, to the next expression, much like GDB's `si` command, except this will break at specific Let expressions
- Typing `Q` will let you go back and either enter a new Lettuce program, or you can quit entirely.


Now, once you've entered either a specific expression, you can view more details about it. With our example program, if we step to 0, we'll see:


If we select the `step` option, we'll start at the first (or 0th) expression. This produces the output:
~~~
--------------------------
-- Step: 0
-- Top Level Expression:
        TopLevel(Let(x,ConstNum(20.0),Let(f,FunDef(List(x),Minus(Ident(x),ConstNum(45.0))),FunCall(Ident(f),List(Ident(x))))))

currN: 0, eB: EmptyExpression

-- Returned From Break Value:
	(v = BreakValue(Let(x,ConstNum(20.0),Let(f,FunDef(List(x),Minus(Ident(x),ConstNum(45.0))),FunCall(Ident(f),List(Ident(x))))),EmptyEnvironment,Store(0): { },0,0)):
	Expr: Let(x,ConstNum(20.0),Let(f,FunDef(List(x),Minus(Ident(x),ConstNum(45.0))),FunCall(Ident(f),List(Ident(x)))))

--------------------------
 -- Choose what to view for step 0: --  
     [0] = Back Out
     [1] = Expr
     [2] = Environment
     [3] = Store
     [4] = Step ahead to step 1
      Option:

~~~

From here, you can now view various parts of the program in detail, and examine them even further using these options. The program is paused at this point. The options are all pretty self-explanatory, so we don't go through them all for brevity's sake. However, we should note that here:
 - If you back out by typing `0`, you'll be sent back to the debugger menu, where you can continue or enter a new program
 - If you type `4` you'll step ahead to the next let expression in the same program. If you don't have another let expression the program will simply end.

The rest we'll leave you to explore!


***Important*** - When you're ready to be done with this program and leave, simply type `exit;;` like so:
~~~
  -- Enter NEW Lettuce Program:
 > exit;;
~~~
The program will exit successfully instead of evaluating a new program. **This only works when not in the in-depth debug menu.**


___

As another example, we can try another more complex program:

~~~
let x = newref(20) in
  let y = deref(x) in
    let z = assignref x <- 45 in
       deref(x) - y + z
       ;;

~~~
And again, we get the prompt. Let's step to the second part of the program!
- ***Important Note for references:*** - When you calling an assignref from the `L` menu, you have to use `...assign x <- y ...` not `assignref(x, y)`

~~~
--Would you like to:
   [L] - Enter a specific step/expression of the program?
   [S] - Step ahead starting at 0?
   [Q] - Quit debugging this program?
   (Enter L, S, or Q): L

~~~
~~~
 -- Enter 'e' to break at an expression,
 -- Enter 'n' to break at a line number: n

  Enter non-negative int, Step n = 2
 -- STEPPING TO n = 2
~~~
Now we see our values:

~~~
--------------------------
-- Step: 2
-- Top Level Expression:
        TopLevel(Let(x,NewRef(ConstNum(20.0)),Let(y,DeRef(Ident(x)),Let(z,AssignRef(Ident(x),ConstNum(45.0)),Minus(DeRef(Ident(x)),Plus(Ident(y),Ident(z)))))))

-- Returned From Break Value:
	(v = BreakValue(Let(z,AssignRef(Ident(x),ConstNum(45.0)),Minus(DeRef(Ident(x)),Plus(Ident(y),Ident(z)))),ExtendEnv(Map(y -> Reference(0)),ExtendEnv(Map(x -> Reference(0)),EmptyEnvironment)),Store(1): { (cell: 0 -> value:NumValue(20.0)) },2,2)):
	Expr: Let(z,AssignRef(Ident(x),ConstNum(45.0)),Minus(DeRef(Ident(x)),Plus(Ident(y),Ident(z))))
	--------------------------
 -- Choose what to view for step 2: --  
     [0] = Back Out
     [1] = Expr
     [2] = Environment
     [3] = Store
     [4] = Step ahead to step 3
~~~

And that's all there is to it!


Remember, once you've backed out and quit, to exit the console type

~~~
> exit;;
~~~

TODO: Either fix this or cut it out
~~Test Cases
To run tests type
`sbt test`
Test cases can be examined in the files in the directory
`src/test/scala/edu/colorado/csci3155/LettuceAST/`~~


### Exploring the Source Code

Go to the directory

~~~
src/main/scala/edu/colorado/csci3155/LettuceAST
~~~

#### LettuceConsole

The main function is defined in the file `LettuceConsole.scala`. It implements the prompting from user
and parses/interprets the user inputs.

TODO: add line number support in error messages from parser.

#### LettuceParser

The recursive descent parser using scala combinator parsing library.

#### LettuceAST.scala

The abstract syntax tree

#### Interpreter

`LettuceInterpreter.scala` has the main interpreter code.

`LettuceValue.scala` implements the values used in the interpreter.

`LettuceEnvironment.scala` implements the environment form identifiers to values.

`LettuceStore.scala` uses the store for mutable references.

`ErrorHandling.scala` has the exceptions thrown.

TODO: Missing types/type inference in Lettuce.

TODO: Objects need to be added.
