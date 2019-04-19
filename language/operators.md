Haskell operators, coarsely explained
=====================================

*This is a rough draft I'm writing iteratively as I try to
understand Haskell operators. I give no guarantee of exactness, this is not (as of yet) something intended for other people. My main objetive here is to be able to remember my state of mind from back when I knew almost nothing (the begginer's mind, so to speak), as it seems that experienced Haskellers are prone to forget how someone new to the language thinks. Here every naive thought and sarcastic quip I have gets recorded, examined and left as it is.*

Introduction
------------

After reading many Haskell tutorials and tripping over so many strange-looking operators, I deciced to try and do a file where I recorded my thought process as I learned about those operators and started to use them. This file is intended so that I can explain Haskell and it's operators to myself later because I'm prone to forgetting things, and perhaps later to someone else, with a better edited redaction posted somewhere else.

From helpful guidance from others I got that the correct path to grasp Haskell would be to understand `Functor` first (which would then shed some light into how Haskell uses the dot `.` in an unique way, for example), then `Applicative` and then later `Monad`. Trying to do it in another order or by ignoring them seems to be the cause of much confusion to others as well as me, so I will follow this advice. This document, however, also focuses in the the thought-stream prose that I think leads to having the groundwork that is necessary to understand them.

The start of such groundwork would be (at least for me) some apparently random thoughts on the language:

**First**, a warning and personal caveat on analogies: Haskellers recommend against doing analogies comparing Haskell features with other programming languages or any other kind of entities, because they often lead to false certainties, [especially when it comes to monads](http://dev.stephendiehl.com/hask/#monadic-myths). This seems to be wise advice I will certainly revisit after I've learned more. However, I will still use analogies in this text. They come naturally to me and I guess most people when first learning something, even if they're flawed. The purpose of this document is to trace the path from flawed understanding towards each improvement, no matter how messy it is, so that I can remember the mindset I had when first learning Haskell and thus trace back my steps years or decades from today. 

So, for the previously stated learning purposes I think we should detach ourselves from the idea that analogies will give us complete or even a little-yet-accurate understanding and treat them instead as landmarks in our own understanding. We still get the little dopamine-filled "aha" moment, but we also remain wary of it and willing to dissect it as necessary. Also, we get to remember our previous analogies as we teach others how we improved on them as they inevitably start having their own more-or-less accurate ideas about the language.

**Second**, on function composition: From a not very detailed bird's view of the language, idiomatic Haskell seems to favor function composition a lot, that is, creating complex functions by chaining together simpler functions in different ways and shifting orders. In fact, many of the seemingly strange operators that abound in Haskell are intended to allow or ease this kind function composition, despite how hard to the eye they might seem to the untrained eye (mine certainly would fit that description as of yet!). My skeptic, speculative guess on this, however, would be that Haskell does suffer from a bit of the Perl tradition of condensing stuff so much it then becomes hard to read even for the people who wrote it originally. Perhaps the future will tell me if this proves to be true.

Also, back to functional composition: It's general idea reminds me of the UNIX philosophy, so perhaps it's old yet still shining light can help us understand newer stuff a bit better. Precisely speaking, functional programming does remind me of [these tenets](https://en.wikipedia.org/wiki/Unix_philosophy#Origin): 

* Make each program do one thing well. To do a new job, build afresh rather than complicate old programs by adding new "features".
  * *Instead of programs (assuming an imperative style), in functional programming we'd think in terms of simple functions. "Don't complicate old programs by adding new features" would be sort of equivalent to "avoid side effects"*. 

* Expect the output of every program to become the input to another, as yet unknown, program. Don't clutter output with extraneous information. Avoid stringently columnar or binary input formats. Don't insist on interactive input.

  * *This sorts of reminds me of Haskell and functional programming's desire to chain functions while keeping side effects to a minimum.*

* Design and build software, even operating systems, to be tried early, ideally within weeks. Don't hesitate to throw away the clumsy parts and rebuild them.

  * *Getting the types right when coding something new would be a part of the design process in Haskell, or in other languages like OCaml.*

* Use tools in preference to unskilled help to lighten a programming task, even if you have to detour to build the tools and expect to throw some of them out after you've finished using them.

  * *The tool here would be the type system itself, which is often touted to lighten programming tasks rather than making them harder... **after** you've spent a while understanding it.*

Curiously enough, the comparisons between functional programming and the UNIX philosophy become harder to see the more the latter is "simplified" from what was said in the  Bell System Technical Journal from 1978, so we will refer to it rather than the following interpretations.

Reading type signatures
-----------------------

Understanding Haskell starts with understanding type signatures, and doing that felt difficult for me right from start because Haskell does type signatures in a way that's very different from more popular and imperative languages. In addition to that, the lack of narrative documentation I crashed over and over against when looking for how to use library-dependent examples seems to stem from an attitude that's common amongst haskellers: They expect you to look at the types and understand everything from looking at them, and thus refuse or balk at using "tenous words" to explain their code to others. Also, from hitting my head against it repeatedly I can report back and confirm that trying to learn Haskell through copy and paste, unlike other programming languages, is very much an impossible endeavor. You've really got to understand the principles before trying anything meaningful, and the principles mostly start with type signatures.

In order to learn to read type signatures, let's first take a look at the hypothetical type signature of a function intended to check if a number is a prime number:

```haskell
isPrime :: Int -> Bool
```

The previous type signature is made of the following elements: 

* `isPrime`, the name of the function.
* `::`, which roughly means "what follows is the type signature of the thing with that name".
*  `Int -> Bool`, which is the actual type signature, that indicates that there's an `Int` parameter and a `Bool` output.

So `isPrime` would take an `Int` and return a `Bool` value. Up to here things seem relatively normal, at least for someone coming from imperative languages. Despite showing itself a little in `isPrime`, however, a certain characteristic of Haskell's type signatures doesn't come into full, clear view until we have a function that uses at least two input parameters. Let's look at the type signature of a function that would add two ints and return another one:

```haskell
myCustomAddition :: Int -> Int -> Int
```

In `myCustomAddition`'s signature the first two `Int`'s are input parameters, while the last `Int` is the returned type. That is, there is no additional syntax indicating that the last type is the output type, they form a chain as equals and we interpret it like described. This pattern also extends to functions with more parameters than two, no matter how many they are. This linear, chain-like style of depicting type signatures takes a while to become natural. There is a well-reasoned argumento to why Haskell is like this: This makes function composition easier, but that detail is out of scope for the time being, so let's take Haskell as it is for now. 

One of the benefits of this kind of type signature is that the type signature of function that is received as a parameter (a first class function) is also fully detailed in the signature of the function that will use it. To explain this in more detail I'll assume that you've used the `map` function in another language before, like Python or Javascript. If you haven't used it, please look for how to do it with the programming language you have more experience with and then come back.

So now we're going to take a look at the type signature of the `map` function in Haskell:

```haskell
map :: (a -> b) -> [a] -> [b]
```

> Tip: You can see the type signature of a function or any value in Haskell by going into the `ghci` interpreter and doing `:t something`, It will output the type signature of `something`.

There's three new things in the `map` function signature: Abstract types, functions and lists, so let's check each one in turn:

* `a` and `b` are abstract types wherever they happen, so they could be *any type at all*. That is, the `map` function doesn't really care what type they are, it just ensures that they remain the same..
* `(a -> b)` indicates that the first parameter would be a function that transforms type `a` into type `b`. If it is between parentheses like this then it is a first-class function that is being passed around. This function could be, for example, our previous `isPrime` function, which would transform a number list into a boolean list as it is passed to `map`.
* The second element of the signature, `[a]`, indicates that the second input parameter to `map` is a list of `a`'s.
* The third, last and thus the returned type indicates that the output of `map` will be a list of `b`'s.

We're mostly done now, but there's also one thing that's missing and that we will need to understand some type signatures, so let's illustrate it with another example: In addition to the `map` function in Haskell there's also a `fmap` function that works with lists as well as other data structures. If you're wondering why there are two versions of the same function, that's something we'll detail later. For now, let's look at `fmap`:

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

`fmap` introduces three aditional things to us: `Functor`, the `=>` arrow (which is different than the `->` arrow), and types `f a` and `f b` that contain two things separated with a space, which we hadn't seen before.

In turn:

* `Functor` is a typeclass, which is roughly equivalent (take this with several grains of salt, though!) to an interface or abstract class in other programming languages. That is, `fmap` ensures that `f` should be a `Functor` in order to work, and it also tells us that `fmap` would complain with an error if something that is not a `Functor` is being passed to it as `f`.

* The `=>` arrow serves to delimit a "context" or type constraint area. That is, the stuff at the left of `=>` are constraints on types, and the stuff at the right of `=>` is the actual type signature.

* Regarding `f a` and `f b` ... TODO.

That'd be all for the type signature crash course. While I probably didn't cover everything there is to Haskell's type signatures, this will suffice for us to know what's happening in the following sections.

Preamble to Functors
--------------------

I don't rememer where I read it first, but I'm fairly certain that I once read *somewhere* that functors in Haskell "would be named *Mappables* in other programming languages", that is, that *a functor is something the map function can be used on*, or something to that effect. I've already used the `map` function in Python and Javascript, so my starting idea of it comes from them. The [Haskell wiki](https://wiki.haskell.org/Functor) seems to agree to a certain extent with the "Mappable" idea:

> The Functor typeclass represents the mathematical functor: a mapping between categories in the context of category theory. In practice a functor represents a type that can be mapped over. 

 But *of course* it also mentions category theory, of which I currently understand little to nothing (even if I hope to someday get it). For the time being, there's also [the definition in the Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Functor):

>[The Functor class](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Functor) is the most basic and ubiquitous type class in the Haskell libraries. A simple intuition is that a Functor represents a “container” of some sort, along with the ability to apply a function uniformly to every element in the container. For example, a list is a container of elements, and we can apply a function to every element of a list, using `map`.

That's a bit more understandable for me. Functors, I also gather from tutorials and comments I've received on chats, are closely related to the `fmap` function in Haskell. The first thing that comes to my mind at this precise moment is: "Why `fmap`? Isn't there a `map` function in Haskell, or is it just a naming thing? If both functions exist, would `fmap` differ from the `map` function I've used before in, say, Python, and if so, how?"

It turns out those were good questions to ask myself (even before looking at types!), because  [this StackOverflow link and it's answers](https://stackoverflow.com/questions/6824255/whats-the-point-of-map-in-haskell-when-there-is-fmap) unearth something quite interesting about Haskell: There is both a `map` function and a `fmap` function in Haskell and they *do* behave differently. 

Quoting from the link:

> ... the type of `map` was generalized to cover `Functor` in Haskell 1.3. I.e., in Haskell 1.3 `fmap` was called `map`. This change was then reverted in Haskell 1.4 and `fmap` was introduced. The reason for this change was pedagogical; when teaching Haskell to beginners the very general type of `map` made error messages more difficult to understand. In my opinion this wasn't the right way to solve the problem. *Haskell 98 is seen as a step backwards by some Haskellers (including me), previous versions having defined a more abstract and consistent library. Oh well.*

There's also a comment in favor of the change, if in a nuanced position:

> The `map` and `fmap` has been around for a long time - it was reheated on the Haskell-prime mailing list in August 2006 - haskell.org/pipermail/haskell-prime/2006-August/thread.html. As a counterpoint, I prefer the status quo. To me, it seems valuable that there's a subset of Haskell that corresponds roughly to Miranda. In the UK, Miranda was used as a teaching language for maths students not just computer science students. If that niche isn't already lost to a non-functional language (e.g. Mathematica) I don't see Haskell with a unified `map` filling it

This is the kind of detail that newcomers to Haskell like me prefer to know as early as possible, even if some experts would think that mentioning it "would confuse newbies". Knowing the history and anecdotes of a programming language is, at least to me, an integral part to knowing such language, so it doesn't distract nor diminish my motivation. Also, I'm not a total newbie. I've programmed before. I've endured language changes across versions of Python (2 to 3), PHP (the before-after split on version 7) and some in Javascript (if albeit less). Maybe that's why watching Haskell changing across versions and people having different opinions about it (that is, showing the human side of a mathematically-inspired language) motivates *me* more than, say, the childlish illustrations and *blank-slate* assumptions of some introductions to Haskell. Your mileage may vary, of course, and I'm not disparaging those introductions (I've learned from them!), just annotating a thought-stream.

So, because of *reasons*, `map` and `fmap` are similar yet different in Haskell. Let's take a look at the type signatures now:

* ```map :: (a -> b) -> [a] -> [b]```
* ```fmap :: Functor f => (a -> b) -> f a -> f b```

As a newcomer I do think that `map` is very easy to understand, and I'd agree that the historical change commented earlier does help me. I can see clearly that the input parameter `(a -> b)` is a rather simple function that transforms type `a` into `b` and that input parameter `[a]` is a list of elements with type `a`, where the output of type `[b]` is a list of elements of type `b`. This is the behavior that one can expect in, say, Python. 

But we're learning Haskell, so we have to invest some attention in understanding why `fmap` exists and why it involves `Functor`. Now that we saw the type signatures, let's have an overview of the code. From [another answer in the same SO link as before](https://stackoverflow.com/a/45289276/9930918) we can see that `map` is [implemented in a two-line recursive function in the Haskell codebase](http://hackage.haskell.org/package/base-4.10.0.0/docs/src/GHC.Base.html#map):

```haskell
map _ []     = []
map f (x:xs) = f x : map f xs
```

Whereas `fmap` is a bit more complicated (taken from `:info fmap` in `ghci`):

```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  ...
  -- Defined in ‘GHC.Base’
```

We can see that `map` is a normal function and `fmap` *belongs* to a class `Functor`, like a method would to a class in an object-oriented language (take this analogy with several grains of salt, as usual). The explaination of this difference in the SO answer goes as follows:

> `fmap` is defined as one of the functions whose implementations must be provided by those data types which wish to belong to the `Functor` type class. That means that there can be more than one data types, not only the "list of values" data type, able to provide an implementation for the fmap function. That makes `fmap` applicable to a much larger set of data types: the functors indeed!

So I think I finally grasped something: `fmap` can work on things that aren't necessarily lists *and* on lists, whereas `map` can only work with lists. This opens a new question: *How could another data structure that is not a list be mapped like with `map`-like behavior?* 

My initial guess goes into trees, whose structure *can* be traversed recursively, just not with a `map` implementation that's dependent on list-specific pattern matching, and I think that answers the question for at least one case.

Don't fret if you still don't know what pattern matching is, the important bit of information here is that Haskell's `map` implementation assumes that it will work on a  list, whereas `fmap` can work on anything that belongs to the class `Functor`. In other words: The `map` function in Haskell is a *very list-specific* implementation of the conceptual/mathematical map operation, whereas `fmap` stands closer to the high theory and so it can be used on many different kinds of structures.

Using functors
--------------

Let's get to the code. From [this post](https://dkalemis.wordpress.com/2014/02/11/examples-of-functors-in-haskell/) we can get ourselves some minimal, runnable Haskell that defines and uses a functor of our own:

```haskell
module Main where
 
data MyFunctor a = MySomething a deriving (Show)
 
instance Functor MyFunctor where
   fmap f (MySomething x) = MySomething (f x)
 
main :: IO ()
main  =
   do
      putStrLn "Program begins."
 
      let thing1 = MySomething 45
      print thing1
      print (fmap (*2) thing1)
      print (fmap (+1) thing1)
      print (fmap (:[]) thing1)
      print (fmap (\x -> 2*x+1:[]) thing1)
      print thing1
 
      putStrLn "Program ends."
```

It runs, so it's ok, I guess, but I don't understand some parts of it. What I think I get is that `MyFunctor` seems to be something that holds a type `a` and apparently does nothing else but sort of "wrap" or "contain" around it and then allow `fmap` to run on it. The `instance` keyword is new to me and the `where` statement has confused me for a while (and still does).

TODO: Continue.

Applicatives
------------

TODO

Monads
------

TODO.

> You can see the type signature of a function or any value by going into the `ghci` interpreter, typing `import System.Environment` into in and then doing `:t getArgs`, for example. It will output the type signature. It will error on `>>=` without parentheses, though. You have to type `:t (>>=)` for it to work.

On a chat I was told that:

> `>>=` can be understood as building on `<$>`, aka `fmap :: Functor f => (a -> b) -> f a -> fb`, which you should really understand before attempting to learn about monads.

The dollar operator
--------------------

[Link 1](https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign)


The `$` operator seems to be mostly a convenience operator, as it helps avoiding the use of parentheses. These two lines 
are equivalent:

```
print (fbStringRep (fizzBuzz n))
print $ fbStringRep $ fizzBuzz n
```

Where `fbStringRep` and `fizzBuzz` would be simple functions that receive only one parameter and return a simple value. It is important to notice here that using the `$` makes you have to read code from right-to-left, something that is very awkward at first but becomes easier with time, as the flow of transformations now flow in a straight line.

The dot operator
----------------

While the `.` operator [can be used to avoid using parentheses](https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign), that's a very limited view of it. As expressed on one of the answers of the StackOverflow question:

> The primary purpose of the `.` operator is not to avoid parentheses, but to chain functions. It lets you tie the output of whatever appears on the right to the input of whatever appears on the left. This usually also results in fewer parentheses, but works differently.

So, the dot `.` operator is the function combinator operator. The following three lines are equivalent, the last using the `$` operator to avoid all use of parentheses.

```haskell
putStrLn (show (1 + 1))
(putStrLn . show) (1 + 1)
putStrLn . show $ 1 + 1
```

Keep in mind the type signatures:

* `putStrLn :: String -> IO ()`
* `show :: Show a => a -> String`

At this point it would also be convenient to reflect on the type signatures of both `$` and `.` to compare them:

*  `($) :: (a -> b) -> a -> b`
*  `(.) :: (b -> c) -> (a -> b) -> a -> c`

Here I see that `$` is much simpler to understand, whereas `.` is a bit more involved. `$` seems to reverse parameters, but this would benefit from a more detailed explaination. I'm not sure I can verbalize what `.` does right now (TODO)


Monad-related operators
-----------------------

The `<-`, `>>`, `>>=` and `return` operators seem to be the monad-related operators. Also, the `<-` operator seems to be some sort of syntactic sugar for using the `>>=` operator. Looking at [this tweet](https://twitter.com/IsolaMonte/status/1114182883806257157)
and commentaries to grasp the `>>=` operator, it is explained as "desugaring of do-expressions". The argument being is as follows:

```
This syntax is starting to come natural to me: 
foo arg1 arg2 >>= bar 

Rather than 
res <- foo arg1 arg2 bar res
```

Here comes a very relevant section of [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/#eightfold-path-to-monad-satori) to save the day (and also confirmating that the intention behind the title works wonders!), indicating that these three blocks are equivalent:

```haskell
do
  a <- f           -- f, g, and h are bound to the names a,
  b <- g           -- b, and c. These names are then passed
  c <- h           -- to 'return' to ensure that all values
  return (a, b, c) -- are wrapped in the appropriate monadic
                   -- context

do {               -- N.B. '{}'  and ';' characters are
  a <- f;          --  rarely used in do-notation
  b <- g;
  c <- h;
  return (a, b, c)
  }

f >>= \a ->
  g >>= \b ->
    h >>= \c ->
      return (a, b, c)
```

I kind of remember (TODO: check this) that `\` in an expression like `\c` indicates an anonymous function, which in this case would be an identity function that returns it's only parameter, and whose binding becomes available in nested lambdas through lexical scoping. *I think I got something right there, but I'm not sure how much.*

Trying to bring this situation into the "real world" (which seems to be a rather common phrase and intention for some reason within the Haskell crowd), the first thing that came to my mind was reading command line arguments, for which the simplest way is achieved with a `import System.Environment` statement that gives us the `getArgs` function, among others.

My first and very naive idea was that these two lines processing command line arguments would 
be equivalent:

```haskell
args <- getArgs
getArgs >>= args
```

But they aren't, and so the code fails. So, since we have no idea what we're doing, let's find some code that
*does* work and see if we can learn something from there. From [this other link](
https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples)
and running it from a file we can see that the following lines of code are valid Haskell:

```haskell
import System.Environment

main = do
    getArgs >>= print
    getProgName >>= print
    getEnvironment >>= print
```

This is exactly what we needed, as it reads command line arguments and uses `>>=`. Let's pause for a while and see what we're actually doing
there. I'm doing a **necessary detour** from `>>=` right now because I need to learn more about the type signatures for the stuff being used on that snippet of code, which are as follows:

* `(>>=) :: Monad m => m a -> (a -> m b) -> m b`
* `print :: Show a => a -> IO ()`
* `getArgs :: IO [String]`
* `getProgName :: IO String`
* `getEnvironment :: IO [(String, String)]`

So, the last three get functions receive no parameters and return a monad that "wraps" a list of strings, a string or a list of tuples, all three of which coming from the "dirty & impure" IO world of the devices, operative system and meatbag operator (yours truly!) of the computer that is running beautiful, pure and squeaky-clean Haskell. I am, however, missing a key piece of knowledge here: The `=>` operator is different 
than `->` and it's being used all over the place. I already sort of grasp long chains that use `->` (where the last
type is the returned type) and have a glimpse that it is related to currying, but I have no idea at all what the `(>>=)` or even the `print` type signature is actually doing due to `=>`. Maybe that's because my Haskell knowledge right now is a hodgepodge of stuff I remember from tutorials and book excerpts.

[This SO link](https://stackoverflow.com/questions/22337214/understanding-haskell-type-signatures) helps understanding type signatures without `=>`, in case a refresher is necessary.

Now onto `=>` itself. In the [Haskell wiki](https://wiki.haskell.org/Type_signature), there is a type signature `inc :: Num a => a -> a` that is described as follows:

> `inc` is the variable, `Num a =>` is the context and `a -> a` is its type, namely a function type with the kind `* -> *`

So `=>` seems to be related to some kind of "context" that can exist within a type signature. 

In the [Types and Typeclasses chapter of Learn You a Haskell](http://learnyouahaskell.com/types-and-typeclasses) there's a part that says:

> Everything before the `=>` symbol is called a class constraint 

For the type signature of the equality function `(==) :: (Eq a) => a -> a -> Bool`, the book states that:

> ...the equality function takes any two values that are of the same type and returns a Bool. The type of those two values must be a member of the Eq class (this was the class constraint).

Here `Eq` is a typeclass that imposes restrictions on the type `a`, much like an interface in other programming languages. Another typeclass, `Ord`, is used for types that have ordering. Consider the greater than operator with a type signature `(>) :: (Ord a) => a -> a -> Bool`. 

I think I get the general idea, but I'm a bit confused on it's implications. `=>` seems to be related to both "context" and to "class constraint", which may or may not be synonyms under shifting conditions. Also, I'm wondering if one can restrict more than one argument with this notation. This is a big TODO.

... but I think I can work with this for now. Returning to the actual types we were examining, which were:

* `(>>=) :: Monad m => m a -> (a -> m b) -> m b`
* `print :: Show a => a -> IO ()`
* `getArgs :: IO [String]`
* `getProgName :: IO String`
* `getEnvironment :: IO [(String, String)]`

Now I can sort of imagine that `Monad` and `Show` would be classes/typeclasses (hmm, is there a difference between the two?) that impose certain restrictions on `m` and `a` within `(>>=)` and `print`, respectively.

I think I've got a general idea of what's happening in the five type signatures now, so let's try doing something naive again that I actually attempted to do before writing these last paragraphs. This code will fail:

```haskell
import System.Environment

main = do
    getArgs >>= print
    "what" >>= print
```

I'm guessing that I'd need to "wrap" a string within a monad so that this idea (or something akin to it but better expressed) can work. So the question is now: How can I wrap a string or any value at all inside a monad?

TODO: Continue