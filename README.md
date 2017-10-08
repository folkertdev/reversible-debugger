# haskell-port

A haskell implementation of the µOz abstract machine and the CaReDeb causal-consistent reversible debugger

# Inspiration

* [The CaReDeb debugger](http://www.cs.unibo.it/caredeb/index.html) by Elena Giachino (developer), Ivan Lanese (developer) and Claudio A. Mezzina (main developer).
* [pi-calculus](https://github.com/renzyq19/pi-calculus) by Will de Renzy-Martin.

The semantics of the µOz abstract machine implemented here are detailed in this paper: 

[A Reversible Abstract Machine and Its Space Overhead](http://www.cs.unibo.it/caredeb/vm-oz.pdf)

## Implementation details 

### Parallel evaluation 

Haskell only has lightweight threads, wheras the original CaReDeb debugger uses java's "heavy" threads to perform computation in parallel. 

It turns out that this isn't really a problem and might actually be better: 
Rather than actually making threads, we can keep the programs branching/spawning structure in a tree-like type. 

```haskell
data Task a 
    = Singleton (Thread a)
    | Parallel (Thread a) (Map ThreadName (Task a))


data Thread a = Thread ThreadName (List History) (List a) 
```

During debugging, we don't actually want concurrent/parallel execution. Rather, when we try to advance a thread that is waiting to receive something, we want a nice message. 
Therefore it is probably possible to write a completely pure, deterministic interpreter. 

The current implementation still relies on actual IO channels to setup communication between "threads".

### Variable Scoping 

In the reference implementation, variables are stored globally and are thus available to all threads. I've kept that implementation for now, thereby going against Haskell principles. 
To make the state global, the variable bindings are stored in an MVar, a reference to a mutable piece of state that is passed into each thread to do its computation with. 

A better solution would be to give each thread its own set of variable bindings. 

## Installing and running 

This project uses [stack](https://docs.haskellstack.org/en/stable/README/) as the build tool and for dependency management. Once installed, the program can be run with

```sh
stack build && stack exec reversible-debugger -- send_receive.txt
```
