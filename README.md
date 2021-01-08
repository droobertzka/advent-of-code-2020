# advent-of-code-2020

Solutions to the [Advent of Code 2020](https://github.com/droopert/advent-of-code-2020) challenges (in Haskell).


## How To Run

Steps to run (using Day 1 as example):

```sh
$ stack ghci src/DayOne.hs

*DayOne> partOne
"prints the answer to Day 1 Part 1"

*DayOne> partTwo
"prints the answer to Day 1 Part 2"
```


## Stream Commands

If you're watching livecoding of this, here are some commands to find out more
information:

* `!language` a bit about Haskell and a link to [haskell.org](https://www.haskell.org)
* `!howto` links to my [guide](https://github.com/droopert/notes/blob/main/haskell-setup-mac.md) on setting a Mac up for Haskell development
* `!whyfp` a link to my [presentation](https://www.youtube.com/watch?v=0RsdeOLdkQ4) on why you should learn functional programming
* `!project` links to the Advent of Code 2020 site
* `!challenge` links to the challenge we're trying to solve at the moment
* `!setup` describes what tools and setup we're using
* `!repo` links to this repo, but who cares... you're already here


## Stuffs We Learned

We are still _quite_ new to Haskell, have plenty of room to learn in Vim, and
who knows what other TIL moments we'll have along the way; but this is what
it's all about, so let's keep a running list. This list contains TIL moments
for viewers as well as myself because every TIL is worth noting!

_Note: The list of TILs is organized by the day we worked on it, not days of the Advent of Code._

### Day One
* using infix operators as functions preserves their argument order, while
  applying one argument via parens allows you to apply that argument to either
side
	* e.g.: `isGreaterThanOne = (> 1)` is the same as `isGreaterThanOne' = (<=) 1`
	* and we can also apply an arg on the left side: `divideSixBy = (6 /)` 
* we have a much better understanding of creating data types in Haskell
	* the relationship between the data types we create and their constructors is still pretty confusing
* bash piping is a great way to explain composition and the `$` operator in Haskell to people who aren't familiar
* indentation in Haskell matters in some cases
	* e.g.: everything but the first line of an expression, definition, or type signature should be indented at least 1 level
* occasionally, breaking functions into smaller functions that share bits of logic can result in requiring otherwise unnecessary type signatures (and therefore more code for devs to have to parse)
* Python has destructuring
* we also learned the syntax for lambdas in Python
* applying a relatively simple macro to ~1000 lines in Vim is surprisingly slow
* we can use visual mode to find/replace in Vim
	* e.g.: `vi[:s/\[/\(` replaces `[` with `(` in the selection
* select everything inside a pair of characters with `vi<char>` (e.g. `vi[`)
	* also, include the outer chars with `va<char>`
* we can get zsh-like shell history search functionality via `ctrl + r`

### Day Two
* the `do` keyword from `System.IO` escapes us into being able to make several statements in
  a block, but then requires a `return` statement and wraps the return value in an `IO` (we think)
* leftward skinny arrow `<-` can be used in `do` blocks to bind the value in a Monad to a variable
	* there are [differences between this and `let`](https://discourse.haskell.org/t/difference-between-let-and/1119)
* Haskell is even more picky about whitespace than we thought:
	* sometimes `let varName =` assignments cannot have the value on the next line
	* some lines wouldn't compile until we switched from tabs to spaces
* when reading in files, whitespace and extra blank lines are a big deal
* concatenating two lists in Haskell performs type casting (at least if one list is explicitly typed)
* fat arrow `=>` in a type signature denotes the bound context
	* e.g.: `:t read --> read :: Read a => String -> a`
* `read` takes one argument but then needs to be bound to a type via `::`
	* e.g. `readThree = read "3"; readThree :: Int --> 3`
* getting the element and an index of a list uses the `!!` infix operator
* Haskell seems to inconveniently default to Integers in a lot of cases where we want to use Ints
* the `div` function can be used to drop decimals off of division results

### Day Three
* we can qualify imports!
	* e.g.: `import qualified Data.Text as T`, then reference methods like so: `T.splitOn`
* we learned the basics of the `Data.Text` library
* learned some handy dandy List methods like `elem`, `isSuffixOf`, and `words`
* `case` statements seem to need to use literals
	* we were getting compiler warnings of redundant cases which weren't
	* we also weren't getting warned about non-exhaustive cases

### Day Four
* we learned how fantastical it is to have the help of the [Haskell Language Server](https://github.com/alanz/haskell-language-server) in our IDE
	* alternatively, we learned how silly it is to do development without a language server XD
	* the HLS even teaches us tricks and tips on better syntax
* when looking at Hackage, the version of `base` in the URL tells us the version of the `base` package that a package like [Data.List](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html) is in
* we learned how to tell what is available in Prelude and what isn't!
	* look in your GHC version's [release notes](https://downloads.haskell.org/ghc/8.8.4/docs/html/users_guide/8.8.4-notes.html) and check package version numbers
* we got modules working!! we think we just had the module declaration syntax wrong
	* correct syntax is: `module MyModule (funcToExport) where`
	* it's also noteable that **filename has to match module name**
* some of the AoC problems can be solved with far less logic/code if we think through how to solve them a little more
	* for example: Day 5 would benefit from changing char flags (`'F'`/`'B'`) into 1s and 0s so we could just add them up to find the highest seat ID
* we were having issues with the type system while dividing with `/` and `div` did the trick
	* this is the code we did it with: `div (length nums) 2`
	* referentially, everything looked fine, even when checking types with `:t`
* we got a tip from HLS to use `notElem` where we were using `not . elem` - neat!
* `where` can be used much like a `case` statement, only we can have logic to check conditions in the cases
* we learned what it's like to run into odd lazily evaluated infinite list situations
	* we were `print`-ing one, and the GHCi was hanging and showing the beginning of the list like so `[528`

### Day Five
* we found a way to [split Lists (and Strings) by a predicate](https://hackage.haskell.org/package/split-0.2.3.4/docs/Data-List-Split.html#v:splitWhen)!
	* `Data.List.Split.splitWhen :: (a -> Bool) -> [a] -> [[a]]`
* you cannot import packages that aren't included in the GHC (e.g. `Data.List.Split`) from GHCi
* Cabal is a package manager allowing us to publish packages (and publish packages?)
	* to begin, `mkdir my-project && cd my-project && cabal init`
	* Cabal is not as ergonomic as Stack! This is in terms of configuration, in CLI use, and documentation
	* you **must** have license in your project's root in order to `cabal install`
* Stack helps you create an environment for Haskell development including dependency management
	* to begin, simply `stack new my-project`
	* you can **not** have a number as part of your package name unless there is a letter adjacent to it (e.g. `package-1` is no good, but `package1` is fine)
	* We learned how to install dependencies via the `package.yaml`
	* Stack will create and manage your `my-project.cabal` file for you, which is much easier than doing it yourself (you have to list all your project's files!)
* when using stack and `System.IO.openFile`, the file path **must** be relative to the project root
	* e.g.: `static/InputDayOne.txt`
* we can search [Hoogle](https://hoogle.haskell.org) by type signature!
	* e.g.: `[a] -> (a -> Bool) -> ([a], [a])` will come up with [span](https://hoogle.haskell.org/?hoogle=%5Ba%5D+-%3E+%28a+-%3E+Bool%29+-%3E+%28%5Ba%5D%2C+%5Ba%5D%29)
among other results

### Day Six

Day 6 (on the advent calendar) part 1 and 2 went pretty quick and smooth, so all the hard work and following rabbit holes is starting to pay off!

* we deeply explored the differences between compose (`.`) and pipe (`$`) and resolved these 2 common stubmling blocks:
	* it is easy to read left-to-right and forget that function application takes precedence over pipe and compose
	* it is tempting to use pipe in a function definition that does not explicitly define all its arguments, but it results in a partially applied function getting passed to some other function which is probably expecting some data. E.g.:
            * **Doesn't work**: `incThenConcat = (++) $ map (+1)`
            * `-> Couldn't match expected type ‘[a]’ with actual type ‘[Integer] -> [Integer]’`
            * `-> Probable cause: ‘map’ is applied to too few arguments`
            * **Works**: `incThenConcat = (++) . map (+1)`

### Day Seven

* `Data.List.Split.splitOn` is a very handy shorthand for splitting a string on a substring and throwing away the delimiter
	* e.g.: `splitOn ", " "foo, bar, baz" == ["foo", "bar", "baz"]`
* we also learned `Data.List.span` and `Data.List.isInfixOf`, which are both very handy
* we used our first custom `type` it wasn't really necessary, but it cleaned up a type signature and made it easier to reason about
* we successfully used where in a couple cases where it helped keep the code more readable

### Day Eight

* we learned how much better we do in the morning with a fresh mind
* we learned the value of just using a library instead of rolling our own versions of utility functions
	* see: time wasted on bug in `replaceElement` instead of using `Control.Lens`'s `.~`
* the compiler is OK with non-exhaustive pattern matching
	* there is an option for enable it as a warning or error
	* there is also a -Wall option for enabling all warnings
* it's extremely important to be careful about order of everything you do inside a loop
	* if not ordered properly, it becomes harder to reason about and you may run the loop once more than intended
* `init` is the opposite of `tail`
* there are a couple other linters/formatters we learned about:
	* [fourmolu](https://github.com/parsonsmatt/fourmolu)
	* [ormolu](https://github.com/tweag/ormolu)
* we learned some more `cabal` basics
* learned about scaffolding with `stack`

### Day Nine

* we learned the formula for calculating unique ways to combine elements in a list
	* `nCk = n!/((n - k)! * k!)`
* we learned about `subsequence` which creates all unique sets from a given list
* we can overload functions based on length of lists passed in via `[_,_]` in our argument definition (as opposed to `(x:xs)`
* `wordsBy` is a great list util for splitting a list into chunks
* we finally learned how to use guards (i.e. `|`) in `where` clauses
	* also that the `otherwise` at the end of guards is a reserved keyword

### Day Ten

* when pattern matching with multiple function definitions, order matters
* we explored 3 ways to pattern match:
	* **function overloading:** good to use when you want completely different function bodies or if the match and return values are simple
	* **`where` clauses:** good when logic for conditions is more complex, but must evaluate to a `Bool`, so not as good for simply matching equality or type
	* **switch cases:** good for when conditions are equality or type, but can't be used for more complex logic in conditions
* we got somewhat familiar with Data.Map from containers library
	* we can compare Maps with `==`
	* almost every operation in Data.Map is _O(n)_ or worse
* using backticks to turn a function into an infix is a quick way to `flip` a binary function
* when defining custom data types, we can easily get `show` and `==` with `deriving (Show, Eq)`
* if our data is shaped too differently from the input, it can make things hard to debug
* `jq` is a handy tool for dealing with `JSON` in bash
	* another useful way is via `node -e "console.log(require('foo.json').bar)"`

### Day Eleven

* Haskell has mutable data structures--whaaaaat?
	* e.g.: [Data.Vector.Mutable](https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector-Mutable.html)
* type synonyms allow us to reuse a function type signature (similar to an interface)
	* simply: `type MySignature = String -> Int -> Char`
* `Data.Bifunctor.bimap` is a great way to apply two functions to the two parts of a tuple (or other data structures with 2 elements)
	* e.g.: `bimap (+ 1) (+ 2) (3, 8) == (4, 10)`
* `uncurry` is handy for binary applying functions to tuples
	* e.g.: `map (uncurry (+)) [(1, 2), (2, 3), (3, 4)] == [6, 9]`
* we can save files in Vim to another filename via `:w NewFileName`
* we learned how to use `data` types with getters which greatly cleaned up our code
	* we also learned how to destructure them nicely without needing the getters (e.g. `(Position shipCoords wpCoords)`)
* `iterate` is a great tool for applying a function to some data recursively, N times
	* it creates an infinite list, and then you can access the Nth element (e.g.: `iterate (+ 1) 0 !! 8 == 8`)
* [Yesod](https://www.yesodweb.com/) is a Haskell framework for creating back end web applications. Neat!

### Day Twelve

* Chinese Remainder Theorem and modulus and all sorts of mathy goodness!

### Day Thirteen

* `screen`
* sweng is slang for **s**oft**w**are **eng**ineering -- yep, didn't know that
* `Data.Bits`, `Control.Monad`, and `Data.Char` are all available in the `base` lib
* `Data.Bits` has handy utility functions like `setBit` and `clearBit`
* `zipWith` - we should probably be using this more; it is awesome

### Day Fourteen

* `divRem`, `quotRem`
* putting guards _after_ a `where` allows us to define named functions within functions
	* the convention is to use the name `go`
* we learned the high-level difference between PureScript and Elm
* `curry` can convert a function that takes a tuple into a curried function


## TODO

* Find out the answers to questions noted in each solution file.

