# A sequential debugger for parallel programs

Finding bugs in parallel programs is often hard. Existing debugging tools can't deal with parallel programs very well. 

This project implements a sequential debugger for the MicroOz language: simple parallel language with threads and message channels. 
MicroOz programs can be evaluated forward, but also backward. This debugger makes it easy to not only see what went wrong, 
but to also go back several steps and try alternative 
evaluations (making different choices) to see what exactly went wrong. The debugger is heavily inspired by [CaReDeb](http://www.cs.unibo.it/caredeb).

The code for this project can be found at: [https://github.com/folkertdev/reversible-debugger]

[https://github.com/folkertdev/reversible-debugger]: (https://github.com/folkertdev/reversible-debugger) 

## Architecture overview 

The debugger design is conceptually simple 

* define your language (and a parser for it)
* define a stepping function for one thread 
* define a stepping function for multiple threads
* define debugger functions 

First we need to define what program we're actually debugging. This debugger is written in Haskell, which makes defining a 
data type with many disjoint cases easy with its sum types (also known as tagged unions).

```haskell

data Program 
    = Let { variable :: Identifier, value :: Value, continuation :: Program }
    | If { condition :: BoolExp, thenBody :: Program, elseBody :: Program }
    | Spawn Program
    | Send { channel :: ChannelName, payload :: Identifier } 
    | ...
```

The parser is a very standard recursive descent parser using the [Parsec](https://hackage.haskell.org/package/parsec) library. 

Next we can define a function that makes one step of progress in evaluating a `Program`, conceptually:

```haskell
data Error 
    = UndefinedVariable Identifier
    | ...

stepThread :: VariableBindings 
           -> List Program 
           -> Either Error ( List Program, VariableBindings, List Message )
```

A value of type `VariableBindings` stores the names and values of all variables. MicroOz uses a stack, so we 
pass a `List Program` to represent the thread's instructions. Evaluating the program may fail, so 
we return `Either` and error of type `Error`, or a modified instruction stack, set of variable bindings and a list of messages.

The `Message` is used for side-effects. In this case used to spawn a new thread. 

MicroOz is special because it has a defined backward semantics. We can keep information around to 
make backward steps - essentially unevaluating the program. This is [possible in general (modulo side-effects)(James and Sabry, 2012)](https://www.cs.indiana.edu/~sabry/papers/information-effects.pdf), but very convenient in MicroOz.

```haskell
data History 
    = BoundVariable { variable :: Identifier }
    | If { condition :: BoolExp, verdict :: Bool, otherBody :: Program }
    | Spawned { pid :: PID }
    | Sent { channel :: ChannelName, payload :: Identifier } 
    ...

stepThreadBackward :: VariableBindings 
                   -> List Program 
                   -> Either Error ( List Program, VariableBindings, List Message )
```
_A PID is a process id, a value uniquely identifying a particular thread_ 

For backward steps the messages are used to undo dependencies. For instance, to undo a `Send`, we must make sure the corresponding
receive is also undone. 

Next we use the per-thread primitives to run multiple threads: 

```haskell
data Pool thread = 
    Pool
        { active :: Map PID thread
        , done :: Map PID thread
        , blocked :: Map PID thread
        , uninitialized :: Map PID thread
        }
```
_`Map` is Haskell's name for a dictionary or key-value store_

We borrow the sequential scheduling algorithm from [A Monad for Deterministic Parallelism (Marlow et al. 2011)](https://simonmar.github.io/bib/papers/monad-par.pdf). 
In the forward case, this algorithm will first exhaust the active set, then wake up blocked threads and finally activate uninitialized threads. 
The backward variant will first roll active threads, then blocked ones and finally done ones.

Finally, we can implement debugging primitives based on the pool stepping functions. Functionality like rolling threads or rolling transactions 
over channels are already part of the pool stepping, but we can also define something custom. During debugging it is useful to step through a program quickly, 
so there is a function `run` that makes 10 forward steps. 

Of particular interest for future work is "send/receive normal form": A thread in this form is either exhausted (no instructions left), or its
next instruction is a send or a receive.

These functions are part of a command line interface where one can load a program, and step through it in both the forward and backward direction.


## Project Structure 

This project follows common Haskell practice putting data structures in `src/Data/*.hs` files. In that directory are things like
`Thread`, `Context` and `Identifier`. These files are mostly boilerplate, but putting data structures into separate files makes
it possible to qualify functions: 

```haskell
Thread.new x y z
```

is vastly better than

```haskel
import Thread (new)

-- 100 lines later

new x y z
```

In the `src/` directory we also find `MicroOz.hs` and `MicroOz/Parser.hs`. The latter is the parser for our language, and the former defines the language 
data type and how individual threads can step forward and backward. 

The `Interpreter.hs` file takes these individual thread stepping functions, and uses them to implement a pool of multiple threads. 

For the moment, `src` also contains `Queue.hs` and `SessionType.hs`. These could be in `src/Data`, but haven't been moved there yet because
these files change regularly. 

Beside code there are three configuration files. `reversible-debugger.cabal` specifies required packages and their versions, and `stack.yaml` defines a 
local sandbox that is used while building the project. There is also a `Dockerfile` that is used to build the project into an image that can
be hosted on heroku.

Finally there is `app/Main.hs` which defines the debugger command line interface.

## Technical Lessons

The main issue I struggled with is that _good abstractions are hard to find_. 

Haskell is a language of elegance: when your abstractions are good, writing code is just a delight. When they are not though, you're back to 
["digging a tunnel under Mordor with a screwdriver"](https://gizmodo.com/programming-sucks-why-a-job-in-coding-is-absolute-hell-1570227192/1570452729). 

In particular, I tried to make make use of a type class (akin to an interface in other languages) called `ReversibleLanguage`. 
The code for running multiple threads would be based on the individual thread stepper functions, and some other parameters of the type class. 
General Haskell philosophy would most likely say that this seems like a good idea, but I only found that I now had an abstract 
interface that required more code (first define the language/data type, then the `ReversibleLanguage` instance), and requires many assumptions and ad-hoc decisions.
So for now, if we'd really want another language to also run with the debugger, we'll have to perform a compilation to MicroOz.

Many of the functions are still highly polymorphic, using type class constraints:

```haskell
lookupVariable :: (MonadState VariableBindings m, MonadError Error m) => Identifier -> m Value
```

Opposed to the concrete:

```haskell
lookupVariable :: Identifier -> StateT VariableBindings (Either Error) Value
```

Given that the code is still changing regularly, keeping these functions polymorphic is reasonable, but for a final product I'd rather want 
them to be concrete. 

So, a side-effect of Haskell's obsession with abstractions is that it's sometimes too easy to abstract.  

Maybe part of the problem is that we as a field haven't had a lot of time yet to create elegant programming abstractions around the 
pi-calculus and the problems that it solves. Maybe adding session types makes all the puzzle pieces fall into place. Anyhow, I'm still 
searching for something elegant.
 

## Personal lessons 

Strangely, this is the first project where the requirements actually changed along the way. It's always said that "the customer will change the requirements", but so 
far I've always found these changes totally predictable: changing colors, adding a page; small things. 

This time around I had to really think about how we could translate ideas into code, often turning an idea around in my mind for multiple days. 
Maybe my projects weren't challenging enough before? In any case, it has become clear to me that for hard problems, it really is better 
to think them through well before you touch a keyboard. 

This project has also been my closest experience to (people) working in academia so far. It's bothered me before that our courses actually show 
very little of the anonymous people working in the mysterious offices on the higher floors.
In particular I was supprised by the casualness with which "ooh let's write a paper about that" was used. It had always
seemed to me that papers were a big thing: An official document of hard labor furthering mankind. 

Many good papers are actually short and about small ideas. 


## Conclusion

We have presented a debugger for the MicroOz language, and looked at its concepts and architecture. 
We have also mentioned the strengths and weaknesses of haskell as a language to implement 
such a tool, and the structure of haskell programs.

This project is already used to build implementations of theoretical ideas. Ideas for future work include:

**Session Types**

This work has already started.

The syntax is extended to support assigning a session type to each thread. A session type is a specification of the interactions that a thread may have, in this case
when and what it may send or receive on channels. Every time a thread want's to perform some action on a channel, 
its session type is checked and advanced when the action is allowed, or the thread blocks when the action is not allowed.

Further fiddling with the semantics of the language is likely, to make the debugger relevant to current research.
The goal is to implement a prototype of a language that is reversible and has session types, and then show that 
in such a language a debugger can offer more help than in a typical language. 

**Working with invalid programs**

The error data type currently stores a lot of information, but doesn't present it in a nice way. Writing an interpreter for valid code is easy in comparison
to writing a debugger that must also be able to handle code with bugs. 

Over the last years, several programming languages have seriously improved their error messages. Many of these changes were inspired by the [Elm](http://elm-lang.org/) language,
which I've been working with for years now and will use for the web-interface. Its implementation of [compilers as assistants](http://elm-lang.org/blog/compilers-as-assistants) is so good that many other languages had to follow suit. 

Working with elm has proven that error messages and error feedback can be improved massively if time is devoted to them. 
It would be great if our debugger was not only technically sound, but actually an assistant in debugging invalid programs.

**Visual Debugging**

A command line interface isn't very flexible and hard to use for larger programs: the 
amount of information that could be useful for debugging is so large that there is no way to present it 
clearly on a command line.

That's why it would be great to have a visual way to interact with the debugger. Load your program, step through its execution. When something 
goes awry, an error message is given with context. It is possible to look at specific parts of the program state via collapsible lists. 

## References

* [A Monad for Deterministic Parallelism - Marlow S., Newton R., Peyton Jones S. (2011)](https://simonmar.github.io/bib/papers/monad-par.pdf)
* [Information Effects - James R. P., Sabry A. (2012)](https://www.cs.indiana.edu/~sabry/papers/information-effects.pdf)
* [Compilers as Assistants - Evan Czaplicki (2015)](http://elm-lang.org/blog/compilers-as-assistants) 
* [A Reversible Abstract Machine and Its Space Overhead - Lienhardt M., Lanese I., Mezzina C.A., Stefani JB. (2012)](http://www.cs.unibo.it/caredeb/vm-oz.pdf)
