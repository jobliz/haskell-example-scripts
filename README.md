haskell-example-scripts
=======================

A set of *runnable* examples I'm writing and/or gathering from across the web 
as I learn Haskell.

Environment setup
-----------------

A quick Haskell environment from Ubuntu repositories:

```apt install ghc cabal-install haskell-stack```

*but* [cabal is seemingly old and stack is the replacement to be used preferentially](https://www.fpcomplete.com/blog/2015/06/why-is-stack-not-cabal), **and** the downloaded stack version
is outdated and gives errors, so also do:

```stack upgrade```

An updated binary should be be created at `/home/your_user/.local/bin/stack`, but I'm not entirely
sure how to use stack yet. Big TODO.

Libraries:

```cabal install split regex-posix regex-pcre csv cassava aeson parsec megaparsec parser-combinators```

VS Code IDE setup (TODO: Language server installation and configuration):

* "Haskell Language Server" in VS Code Extensions Marketplace
* [Github Repo of Extension](https://github.com/alanz/vscode-hie-server)

Self-study outline
------------------

TODO: Think this carefully, explain achieved progress.

* Basic language features, function definition, operators, if statements
* Command line input parsing
* Pattern matching in both function definitions and case notation
* Playing around with higher-order functions, maybe some silly things
* Using folds instead of recursion and looping
* Using the type system to create simple data structures, and the algorithms to use them
* Using the type system to create recursive data structures, and the algorithms to use them
* Parsing simple log files using the type system to check each row
* Parsing simple log files using regexes to match data instead of arbitrary string splitting
* Parsing CSV using the type system to check each row
* Parsing a simple mathematical language with only pattern matching and eval
* Parsing a simple mathematical language with parser combinators
* Parsing a simple imperative language with parser combinators

General introductory posts
--------------------------

* [What I Wish I knew When Learning Haskell](
http://dev.stephendiehl.com/hask/)


Online books
------------

* [Real World Haskell](http://book.realworldhaskell.org/read/)
* [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
* [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)

Reddit links
------------

* [Q: How do I approach learning Haskell](https://www.reddit.com/r/haskell/comments/6nllfs/q_how_do_i_approach_learning_haskell/)
* [Learning Haskell + Which book *with* practice problems?](https://www.reddit.com/r/haskell/comments/65gs02/learning_haskell_which_book_with_practice_problems/)

Course sites - some free
------------------------

* [Upenn CIS 194: Introduction to Haskell (lectures available)](http://www.cis.upenn.edu/~cis194/spring13/)
* [Upenn CIS 552: Advanced Programming (only homework available)](http://cis.upenn.edu/~cis552/current/homework.html)

Online tutorials on functional programming with Haskell
-------------------------------------------------------

* [10 things I learnt diving in the functional programming deep end - with Haskell](https://making.pusher.com/10-things-i-learnt-diving-in-the-functional-programming-deep-end-with-haskell/)
* [Folding (prelude): A Haskell quick start](https://davesquared.net/2012/02/haskell-newbie-attempts-a-haskell-quick-start.html)
* [Folding, part 1: From recursion to folding](https://davesquared.net/2012/02/folds-pt1-from-recursion-to-folds.html)
* [Folding, part 2: From loops to folds](https://davesquared.net/2012/02/folds-pt2-from-loops-to-folds.html)
* [Folding, part 3: Left fold, right?](https://davesquared.net/2012/03/folds-pt3-left-fold-right.html)

Online tutorials on Haskell and libraries
-----------------------------------------

* [The Pragmatic Haskeller (recipes to build a simple web app to manage recipes)](https://github.com/cakesolutions/the-pragmatic-haskeller)
* [Regarding the above, the posts are actually here](https://www.schoolofhaskell.com/user/adinapoli/the-pragmatic-haskeller)
* [Introduction to parsing with Parsec](http://jakewheat.github.io/intro_to_parsing/)
* [Regarding, the above, discussion in Reddit](https://www.reddit.com/r/haskell/comments/2ia5u2/after_some_failed_attempts_to_learn_parsec_i_came/)

Simple imperative language parser links
---------------------------------------

* [Haskell wiki: Parsing a simple imperative language](https://wiki.haskell.org/Parsing_a_simple_imperative_language)
* [Megaparsec example: Parsing a simple imperative language](https://github.com/mrkkrp/markkarpov.com/blob/master/megaparsec/parsing-simple-imperative-language.md)
* [LIS-Parser: Parser para LIS](https://github.com/arypbatista/LIS-Parser) 

Symbolic math
=============

* [Symbolic math and Haskell](https://www.reddit.com/r/haskell/comments/qm6j5/symbolic_math_and_haskell/)
* [Haskell library like SymPy?](https://stackoverflow.com/questions/3295345/haskell-library-like-sympy)

Other links
-----------

http://pleac.sourceforge.net/pleac_haskell/index.html