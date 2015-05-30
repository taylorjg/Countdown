using System;
using System.Collections.Generic;
using System.Linq;

// ReSharper disable PossibleMultipleEnumeration

namespace Countdown1
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
            public abstract Value Value();
        };

        private class Num : Expr
        {
            private readonly int _value;

            public Num(Value value)
            {
                _value = value;
            }

            public override Value Value()
            {
                return _value;
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

            public override Value Value()
            {
                return Apply(_op, _e1.Value(), _e2.Value());
            }
        };

        private static Value Apply(Op op, Value v1, Value v2)
        {
            switch (op)
            {
                case Op.Add:
                    return v1 + v2;
                case Op.Sub:
                    return v1 - v2;
                case Op.Mul:
                    return v1 * v2;
                case Op.Div:
                    return v1 / v2;
                default:
                    throw new ArgumentException("Unknown operation", "op");
            }
        }

        private static bool Legal(Op op, Value v1, Value v2)
        {
            switch (op)
            {
                case Op.Add:
                    return true;
                case Op.Sub:
                    return v2 < v1;
                case Op.Mul:
                    return true;
                case Op.Div:
                    return v1 % v2 == 0;
                default:
                    throw new ArgumentException("Unknown operation", "op");
            }
        }

        private static IEnumerable<Tuple<IEnumerable<Value>, IEnumerable<Value>>> Unmerges(IEnumerable<Value> values)
        {
            var x = values.ElementAt(0);
            var singletonX = new[] { x }.AsEnumerable();

            if (values.Take(3).Count() == 2)
            {
                var y = values.ElementAt(1);
                var singletonY = new[] { y }.AsEnumerable();
                return new[] { Tuple.Create(singletonX, singletonY), Tuple.Create(singletonY, singletonX) };
            }

            Func<Tuple<IEnumerable<Value>, IEnumerable<Value>>, IEnumerable<Tuple<IEnumerable<Value>, IEnumerable<Value>>>> addX =
                t =>
                {
                    var ys = t.Item1;
                    var zs = t.Item2;
                    return new[] { Tuple.Create(singletonX.Concat(ys), zs), Tuple.Create(ys, singletonX.Concat(zs)) };
                };

            var xs = values.Skip(1);
            var firstBit = new[] { Tuple.Create(singletonX, xs), Tuple.Create(xs, singletonX) };
            var secondBit = Unmerges(xs).SelectMany(addX);
            return firstBit.Concat(secondBit);
        }

        private static readonly Op[] Ops = { Op.Add, Op.Sub, Op.Mul, Op.Div };

        private static IEnumerable<Tuple<Expr, Value>> Combine(Tuple<Expr, Value> ev1, Tuple<Expr, Value> ev2)
        {
            var e1 = ev1.Item1;
            var v1 = ev1.Item2;
            var e2 = ev2.Item1;
            var v2 = ev2.Item2;

            return
                from op in Ops
                where Legal(op, v1, v2)
                select Tuple.Create(new App(op, e1, e2) as Expr, Apply(op, v1, v2));
        }

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
            Console.WriteLine("SubSeqs(numbers).SelectMany(MkExpr).Count(): {0}", SubSeqs(numbers).SelectMany(MkExpr).Count());
            var answer = Countdown(832, numbers);
            Console.WriteLine("answer: {0}", answer);
        }
    }
}
