using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

// ReSharper disable PossibleMultipleEnumeration

namespace Countdown3
{
    using Value = Int32;

    internal static class Program
    {
        private enum Op
        {
            Add,
            Sub,
            Mul,
            Div
        };

        private abstract class Expr
        {
        };

        private class Num : Expr
        {
            private readonly int _value;

            public Num(Value value)
            {
                _value = value;
            }

            public override string ToString()
            {
                return Convert.ToString(_value);
            }
        };

        private class App : Expr
        {
            private readonly Op _op;
            private readonly Expr _e1;
            private readonly Expr _e2;

            public App(Op op, Expr e1, Expr e2)
            {
                _op = op;
                _e1 = e1;
                _e2 = e2;
            }

            public Op Op
            {
                get { return _op; }
            }

            private readonly static IDictionary<Op, string> OpsToSymbols = new Dictionary<Op, string>
            {
                {Op.Add, "+"},
                {Op.Sub, "-"},
                {Op.Mul, "*"},
                {Op.Div, "/"}
            };

            public override string ToString()
            {
                return string.Format("({0}{1}{2})", _e1, OpsToSymbols[_op], _e2);
            }
        };

        private static IEnumerable<Tuple<IEnumerable<Value>, IEnumerable<Value>>> Unmerges(IEnumerable<Value> values)
        {
            var x = values.ElementAt(0);
            var singletonX = new[] { x }.AsEnumerable();

            if (values.Take(3).Count() == 2)
            {
                var y = values.ElementAt(1);
                var singletonY = new[] { y }.AsEnumerable();
                return new[] { Tuple.Create(singletonX, singletonY) };
            }

            Func<Tuple<IEnumerable<Value>, IEnumerable<Value>>, IEnumerable<Tuple<IEnumerable<Value>, IEnumerable<Value>>>> addX =
                t =>
                {
                    var ys = t.Item1;
                    var zs = t.Item2;
                    return new[] { Tuple.Create(singletonX.Concat(ys), zs), Tuple.Create(ys, singletonX.Concat(zs)) };
                };

            var xs = values.Skip(1);
            var firstBit = new[] { Tuple.Create(singletonX, xs) };
            var secondBit = Unmerges(xs).SelectMany(addX);
            return firstBit.Concat(secondBit);
        }

        private static bool Non(Op op, Expr e)
        {
            if (e is Num) return true;
            var app = (App) e;
            return op != app.Op;
        }

        private static IEnumerable<Tuple<Expr, Value>> Comb1(Expr e1, Value v1, Expr e2, Value v2)
        {
            if (Non(Op.Sub, e1) && Non(Op.Sub, e2))
            {
                if (Non(Op.Add, e2))
                {
                    yield return Tuple.Create(new App(Op.Add, e1, e2) as Expr, v1 + v2);
                }

                yield return Tuple.Create(new App(Op.Sub, e2, e1) as Expr, v2 - v1);
            }

            if (1 < v1 && Non(Op.Div, e1) && Non(Op.Div, e2))
            {
                if (Non(Op.Mul, e2))
                {
                    yield return Tuple.Create(new App(Op.Mul, e1, e2) as Expr, v1 * v2);
                }

                var q = v2 / v1;
                var r = v2 % v1;

                if (r == 0)
                {
                    yield return Tuple.Create(new App(Op.Div, e2, e1) as Expr, q);
                }
            }
        }

        private static IEnumerable<Tuple<Expr, Value>> Comb2(Expr e1, Value v1, Expr e2, Value v2)
        {
            if (Non(Op.Sub, e1) && Non(Op.Add, e2) && Non(Op.Sub, e2))
            {
                yield return Tuple.Create(new App(Op.Add, e1, e2) as Expr, v1 + v2);
            }

            if (1 < v1 && Non(Op.Div, e1) && Non(Op.Div, e2))
            {
                if (Non(Op.Mul, e2))
                {
                    yield return Tuple.Create(new App(Op.Mul, e1, e2) as Expr, v1 * v2);
                }

                yield return Tuple.Create(new App(Op.Div, e1, e2) as Expr, 1);
            }
        }

        private static IEnumerable<Tuple<Expr, Value>> Combine(Tuple<Expr, Value> ev1, Tuple<Expr, Value> ev2)
        {
            var e1 = ev1.Item1;
            var v1 = ev1.Item2;
            var e2 = ev2.Item1;
            var v2 = ev2.Item2;
            if (v1 < v2) return Comb1(e1, v1, e2, v2);
            if (v1 == v2) return Comb2(e1, v1, e2, v2);
            return Comb1(e2, v2, e1, v1);
        }

        // nearest :: Value -> [(Expr, Value)] -> (Expr, Value)
        // nearest n (ev@(_, v):evs) =
        //     if d == 0 then ev
        //     else search n d ev evs
        //     where d = abs (n - v)
        private static Tuple<Expr, Value> Nearest(Value n, IEnumerable<Tuple<Expr, Value>> evs)
        {
            var evsHead = evs.First();
            var evsTail = evs.Skip(1);
            var v = evsHead.Item2;
            var d = Math.Abs(n - v);
            return (d == 0) ? evsHead : Search(n, d, evsHead, evsTail);
        }

        // search :: Value -> Value -> (Expr, Value) -> [(Expr, Value)] -> (Expr, Value)
        // search _ _ ev [] = ev
        // search n d ev ((e, v):evs)
        //     | d' == 0 = (e, v)
        //     | d' < d = search n d' (e, v) evs
        //     | d' >= d = search n d ev evs
        //     where d' = abs (n - v)
        private static Tuple<Expr, Value> Search(Value n, Value d, Tuple<Expr, Value> ev, IEnumerable<Tuple<Expr, Value>> evs)
        {
            var evsHead = evs.FirstOrDefault();

            if (evsHead == null)
            {
                return ev;
            }


            var evsTail = evs.Skip(1);
            var v = evsHead.Item2;
            var ddash = Math.Abs(n - v);
            if (ddash == 0) return evsHead;
            if (ddash < d) return Search(n, ddash, evsHead, evsTail);
            return Search(n, d, ev, evsTail);
        }

        private static IEnumerable<Tuple<Expr, Value>> MkExpr(IEnumerable<Value> numbers)
        {
            var x = numbers.First();
            var tail = numbers.Skip(1);

            if (!tail.Any()) return new[] { Tuple.Create(new Num(x) as Expr, x) };

            return
                from pair in Unmerges(numbers)
                let ys = pair.Item1
                let zs = pair.Item2
                from ev1 in MkExpr(ys)
                from ev2 in MkExpr(zs)
                from ev in Combine(ev1, ev2)
                select ev;
        }

        private static IEnumerable<IEnumerable<Value>> SubSeqs(IEnumerable<Value> numbers)
        {
            var x = numbers.First();
            var xs = numbers.Skip(1);
            var singletonX = new[] { x };

            if (!xs.Any())
            {
                return new[] { singletonX };
            }

            var xss = SubSeqs(xs);

            return xss.Concat(new[] { singletonX }.Concat(xss.Select(ys => singletonX.Concat(ys))));
        }

        private static Tuple<Expr, Value> Countdown(Value n, IEnumerable<Value> numbers)
        {
            return Nearest(n, SubSeqs(numbers).SelectMany(MkExpr));
        }

        private static void Main()
        {
            var numbers = new[] { 1, 3, 7, 10, 25, 50 };
            var sw = new Stopwatch();
            sw.Start();
            var answer = Countdown(832, numbers);
            sw.Stop();
            Console.WriteLine("{0} = {1} ({2}ms)", answer.Item1, answer.Item2, sw.ElapsedMilliseconds);
        }
    }
}
