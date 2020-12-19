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
* we can search [Hoogle] by type signature!
	* e.g.: `[a] -> (a -> Bool) -> ([a], [a])` will come up with [span](https://hoogle.haskell.org/?hoogle=%5Ba%5D+-%3E+%28a+-%3E+Bool%29+-%3E+%28%5Ba%5D%2C+%5Ba%5D%29)
among other results



## TODO

* Make a calendar so we can cross off each day because that's the most satisfying part!
* Figure out how to run a Haskell program with modules so we can take advantage of the file IO solution we came up with instead of parsing each input set.
* Find out the answers to questions noted in each solution file.

