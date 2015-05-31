## Description

Playing around with "The _Countdown_ problem" (pearl number 20) from _Pearls of Functional Algorithm Design_ by Richard Bird.

## TODO

* Type in all 5 versions of the Haskell code from the book
    * ~~countdown1~~
    * ~~countdown2~~
    * ~~countdown3~~
    * countdown4
    * countdown5
* Try it in C#
    * ~~countdown1~~
    * ~~countdown2~~
    * ~~countdown3~~
    * countdown4
    * countdown5
* Add a GUI front end to the Haskell code - maybe in Elm ?
* Add a GUI front end to the C# code - maybe in WPF ?

## Remarks

I encountered a problem when porting the code to C#. It ran for ages without giving a result.
The problem seemed to be in the <code>Nearest</code> function. Eventually, I re-implemented this function using a simple loop and it then worked fine - in fact, it ran really quickly. I think the problem may be that the Haskell version of <code>Nearest</code> uses a helper function called <code>Search</code> which is tail recursive. I suspect that C# does not optimise tail calls. I'm not sure whether this accounts for the problem that I was seeing.

Here is a screenshot of the C# version of Countdown2:

![Countdown2 (C#)](https://raw.githubusercontent.com/taylorjg/Countdown/master/Images/Countdown2_CSharp_Screenshot.png "Countdown2 (C#)")

## Links

* [_Pearls of Functional Algorithm Design_](http://www.cambridge.org/gb/academic/subjects/computer-science/programming-languages-and-applied-logic/pearls-functional-algorithm-design)
* [Notation used in 'Pearls of Functional Algorithm Design'](http://programmers.stackexchange.com/questions/118869/notation-used-in-pearls-of-functional-algorithm-design)
* [The Z Notation: A Reference Manual](http://spivey.oriel.ox.ac.uk/~mike/zrm/zrm.pdf)
* [How To Read _Pearls of Functional Algorithm Design_](http://www.atamo.com/blog/how-to-read-pearls-by-richard-bird-1/)
* [Elm](http://elm-lang.org/)
