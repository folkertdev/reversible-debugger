
```haskell
data StackF a next 
    = Push a next
    | Pop (a -> next)
    | End 

type Stack a = Free StackF a
     
program :: Stack Int
program = do 
    push 5 
    push 4 
    a <- pop
    b <- pop
    push (a + b)

push :: a -> Stack a ()
push v = liftFree (Push v ()) 

pop :: Stack a a 
pop = liftFree (Pop identity)

end :: Stack a a 
end = liftFree End

foldFree :: Functor f => (f a -> a -> a) -> Free f a -> a -> a 
foldFree step instruction default = 
    case instruction of 
        Pure v -> 
            
            
evaluate :: Stack a -> State (List a) ()
evaluate instruction = 
    case instruction of 
        Free (Push value rest) -> do
            State.modify (\stack -> value : stack)
            evaluate rest

        Free (Pop continuation) -> do
            stack <- State.get 
            case stack of 
                [] -> error "empty stack"
                x:xs -> do
                    State.put xs
                    continuation x
                    
        Pure value -> 
            return ()
```


```haskell
data Expr
    = Literal Int
    | Add Expr Expr 
```

`Literal` is the only constructor that can occur as a leaf, and `Add` is the only node.
Using `Fix` We can equivalently write 

```haskell
data ExprF next
    = Literal Int
    | Add next next 

simple :: Fix ExprF
simple = Fix (Literal 42)

complex :: Fix ExprF
complex = 
    Fix (Add (Fix (Literal 40)) (Fix (Literal 2)))
```

By decoupling the recursion from the content, we can write functions that deal with only one level 
of the tree and apply them to the full tree. For instance evaluation of the above expression can be written as 

```haskell
evaluate :: Fix ExprF -> Int 
evaluate = 
    Fix.cata $ \expr -> 
        case expr of 
            Literal v -> 
                v

            Add a b -> 
                a + b
```

The `cata` function - a catamorphism also known as a fold or reduce - applies evaluate from the bottom up. In the code we write, we 
only need to make local decisions and don't have to write the plumbing to get the recursion right. 




## Extracting recursion: Fix and Free 

the `Fix` and `Free` data types are two ways of factoring out recursion from a data type definition. 
Both require the data type to be an instance of `Functor`: The type is of the shape `f a` - like `List a` or `Maybe a`, and there exists a mapping function `fmap :: (a -> b) -> (f a -> f b)`. 

Fix requires the data type to have a natural leaf: a constructor that does not contain an `a`. `Free` on the other hand lets us choose some other type for the leaves.

## Fix 

The `Fix` data type is the fixed point type. 

```haskell
data Fix f = Fix (f (Fix f))
```

It allows us to express a type of the shape `f (f (f (f (..))))` concisely. For the 
values of this type to be finite, the `f` must have a constructor that does 
not recurse to be a leaf. Take for instance this simple expression language

## Wrapping ProgramF in Free 

The Free monad is very similar to `Fix`, but allows us to use a different type for the leaves, and 
enables us to use do-notation. Writing expressions with `Fix` can be quite messy, free monads 
allow us to write examples much more succinctly.

```haskell
data Free f a
    = Pure a 
    | Free (f (Free f a))
```



some notes 

* I think my understanding of 'location' is not completely correct
* I think I've missed program recursion, because it can be expressed as function recursion. Not 
sure whether this is a problem.
* could let inline its value into its body? Of course this is not practical in "real" programming languages
because of the memory overhead, but here it should be fine and restricts the variables only to 
stuff we get from a receive.

This is much like let-bindings compile into lambda terms in the lambda calculus, where 
```
let x = 4 in y + x

-- Gets transformed into 

(\x -> y + x) 4
```

The lambda calculus can use the above to compile let-bindings away completely. For the pi-calculus that 
won't quite work because values that are `receive`d are not yet available 

