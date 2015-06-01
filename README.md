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

## Porting to C# from Haskell

I encountered a problem when porting the code to C#. It ran for ages without giving a result.
The problem was with <code>Nearest</code> / <code>Search</code>. I was using <code>Skip(1)</code>
in a couple of places to get the tail of the enumerable of <code>Tuple&lt;Expr, Value&gt;</code>.
I think this was causing multiple enumerations which was dreadful for performance.
I changed the implementation to directly use an enumerator and this fixed the problem in Countdown3:

```C#
private static Tuple<Expr, Value> Nearest(Value n, IEnumerable<Tuple<Expr, Value>> evs)
{
    using (var e = evs.GetEnumerator())
    {
        e.MoveNext();
        var evsHead = e.Current;
        var v = evsHead.Item2;
        var d = Math.Abs(n - v);
        return (d == 0) ? evsHead : Search(n, d, evsHead, e);
    }
}

private static Tuple<Expr, Value> Search(Value n, Value d, Tuple<Expr, Value> ev, IEnumerator<Tuple<Expr, Value>> e)
{
    if (!e.MoveNext())
    {
        return ev;
    }

    var evsHead = e.Current;
    var v = evsHead.Item2;
    var ddash = Math.Abs(n - v);
    if (ddash == 0) return evsHead;
    if (ddash < d) return Search(n, ddash, evsHead, e);
    return Search(n, d, ev, e);
}
```

However, in Countdown1 and Countdown2, it caused a <code>StackOverflowException</code> due to the recursion in <code>Search</code>.
For Countdown1, the stack overflow occurred in both Debug and Release builds.
For Countdown2, the stack overflow occurred in Debug build but not in Release build.
I'm guessing that the stack frames are smaller in Release build so more of them fit into the available stack space.
Anyway, I changed the implementation of <code>Nearest</code> to use a simple loop instead.
This fixed the problem although it did not run any quicker.

```C#
private static Tuple<Expr, Value> Nearest(Value n, IEnumerable<Tuple<Expr, Value>> evs)
{
    Tuple<Expr, Value> result = null;
    var currentBestDiff = int.MaxValue;

    foreach (var ev in evs)
    {
        var thisDiff = Math.Abs(n - ev.Item2);
        if (thisDiff == 0) return ev;
        if (thisDiff < currentBestDiff)
        {
            currentBestDiff = thisDiff;
            result = ev;
        }
    }

    return result;
}
```

Here is a screenshot of the C# version of Countdown3:

![Countdown3 (C#)](https://raw.githubusercontent.com/taylorjg/Countdown/master/Images/Countdown3_CSharp_Screenshot.png "Countdown3 (C#)")

## Links

* [_Pearls of Functional Algorithm Design_](http://www.cambridge.org/gb/academic/subjects/computer-science/programming-languages-and-applied-logic/pearls-functional-algorithm-design)
* [Notation used in 'Pearls of Functional Algorithm Design'](http://programmers.stackexchange.com/questions/118869/notation-used-in-pearls-of-functional-algorithm-design)
* [The Z Notation: A Reference Manual](http://spivey.oriel.ox.ac.uk/~mike/zrm/zrm.pdf)
* [How To Read _Pearls of Functional Algorithm Design_](http://www.atamo.com/blog/how-to-read-pearls-by-richard-bird-1/)
* [Elm](http://elm-lang.org/)
