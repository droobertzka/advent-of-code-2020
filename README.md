# advent-of-code-2020

Solutions to the [Advent of Code 2020](https://github.com/droopert/advent-of-code-2020) challenges (in Haskell).


## Completed

`X` marks part 1 and 2 complete. `/` marks part 1 complete.

* `X` - Day 1
* `/` - Day 2
* That's it so far. We got started late, so give us a break...


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


## TODO

* Make a calendar so we can cross off each day because that's the most satisfying part!

