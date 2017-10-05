# haskell-port

A haskell implementation of the µOz abstract machine and the CaReDeb causal-consistent reversible debugger

# Inspiration

* [The CaReDeb debugger](http://www.cs.unibo.it/caredeb/index.html) by Giachino (developer), Ivan Lanese (developer) and Claudio A. Mezzina (main developer).
* [pi-calculus](https://github.com/renzyq19/pi-calculus) by Will de Renzy-Martin.

The semantics of the µOz abstract machine implemented here are detailed in this paper: 

[A Reversible Abstract Machine and Its Space Overhead](http://www.cs.unibo.it/caredeb/vm-oz.pdf)

# Parallel evaluation 

Haskell only has lightweight threads, wheras the original CaReDeb debugger uses java's "heavy" threads to perform computation in parallel. 

It turns out that this isn't really a problem and might actually be better: 
Rather than actually making threads, we can keep the programs branching/spawning structure in a tree-like type. 


    data Task a 
        = Singleton (Thread a)
        | Parallel (Task a) (Task a)

    data Thread a = Thread ThreadName (List History) (List a) 


The Thread datatype stores the information of an individual thread: its name, history steps (to go back in time) and remaining program steps. 
The Task datatype encodes the branching structure of the program. The java implementation needs to keep separate accounts of the parent-child thread structure: this indirection can cause subtle bugs.

When a parallel Task needs to be evaluated, the two branches are each advanced in their own lightweight thread, then the result is merged back into the Task data structure living in the main thread.

# Variable Scoping 

In the reference implementation, variables are stored globally and are thus available to all threads. I've kept that implementation for now, thereby going against Haskell principles. 
To make the state global, the variable bindings are stored in an MVar, a reference to a mutable piece of state that is passed into each thread to do its computation with. 

The MVar type has built-in locking, preventing concurrent updates (which could cause race conditions). 

A better solution would be to give each thread its own set of variable bindings. 

# Caching of previous states 

My current implementation builds strictly on the reverse semantics. The reference implementation uses some caching to quickly get back to earlier program states, but I think 
this is a premature optimization: the language is pretty simple, and even reversing many steps should take very little time. 

