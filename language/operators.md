Haskell operators, coarsely explained
=====================================

*This is a rough draft I'm writing iteratively as I try to
understand Haskell operators. I give no guarantee of exactness, this is not (as of yet) something intended for other people. My main objetive here is to be able to remember my state of mind from back when I knew almost nothing (the begginer's mind, so to speak), as it seems that experienced Haskellers are prone to forget how someone new to the language thinks. Here every naive thought and sarcastic quip I have gets recorded, examined and left as it is.*

Some links that would be convenient to read before this:

* [Learning Haskell by Type](https://www.holger-peters.de/haskell-by-types.html)
* [Stack Overflow: What is the purpose of wrapped values in Haskell](https://softwareengineering.stackexchange.com/questions/303472/what-is-the-purpose-of-wrapped-values-in-haskell)

Introduction
------------

After reading many Haskell tutorials and tripping over so many strange-looking operators, I deciced to try and do a file where I recorded my thought process as I learned about those operators and started to use them. While this file is intended so that I can explain operators to myself later because I'm prone to forgetting things (and perhaps later to someone else, with a better edited readaction posted somewhere else), there are principles that I should keep in mind before thinking about operators. 

From the aforementioned tutorials I got (without much learning structure or discipline) to a point where I wanted to play around with the `>>=` operator because it made command line argument reading easier and I do like working with the command line... but it turns out that the `>>=` operator is related to monads, which are themselves better understood if other Haskell features are understood first.

On a chat I was told that:

> `>>=` can be understood as building on `<$>`, aka `fmap :: Functor f => (a -> b) -> f a -> fb`, which you should really understand before attempting to learn about monads.

So the learning path would be `Functor` -> `Applicative` -> `Monad`, and learning `Functor` would bring insight into the dot operator, for example (an insight I don't have right now).

A warning on analogies: Haskellers recommend against doing analogies comparing Haskell features with other programming languages or any other kind of entities because they lead to false certainties, [especially when it comes to monads](http://dev.stephendiehl.com/hask/#monadic-myths). This seems to be wise advice I will certainly revisit after I've learned more. However, I will still use analogies in this text. They come naturally to me and I guess most people when first learning something, even if they're flawed. The purpose of this document is to trace the path from flawed understanding towards each improvement, no matter how messy it is, so that I can remember the mindset I had when first learning Haskell and thus trace back my steps years or decades from today. So, for this purpose and for this text I think we should detach ourselves from the idea that analogies will give us complete or even a little-yet-accurate understanding and treat them instead as landmarks in our own understanding. We still get the little dopamine-filled "aha" moment, but we also remain wary of it and willing to dissect it as necessary.

Reading type signatures
-----------------------

TODO: Put everything related to how to read type signatures here.

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

```
map _ []     = []
map f (x:xs) = f x : map f xs
```

Whereas `fmap` is a bit more complicated (taken from `:info fmap` in `ghci`):

```
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

TODO

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

```
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

```
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

```
args <- getArgs
getArgs >>= args
```

But they aren't, and so the code fails. So, since we have no idea what we're doing, let's find some code that
*does* work and see if we can learn something from there. From [this other link](
https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples)
and running it from a file we can see that the following lines of code are valid Haskell:

```
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

> You can see the type signature of a function by going into the `ghci` interpreter, typing `import System.Environment` into in and then doing `:t getArgs`, for example. It will output the type signature. It will error on `>>=` without parentheses, though. You have to type `:t (>>=)` for it to work.

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

```
import System.Environment

main = do
    getArgs >>= print
    "what" >>= print
```

I'm guessing that I'd need to "wrap" a string within a monad so that this idea (or something akin to it but better expressed) can work. So the question is now: How can I wrap a string or any value at all inside a monad?

TODO: Continue
