/**
 * JavaScript from Scratch
 *
 *   -- Ashok Menon (@amnn)
 */

// TODO: Comments
// TODO: Contents Page + Tags
// TODO: Presentation skeleton

"use strict";





/** Combinators
 *
 * A combinator is a higher-order function that uses only function application,
 * abstraction, its parameters, and previously defined combinators in computing
 * its result [1] (Note, this precludes explicit recursion!).
 *
 * We make extensive use of JavaScript's arrow functions. Their parameters are
 * surrounded by `(...)`, and their body is prefixed by `=>`. If their body
 * consists of only a single returned expression, then we omit the curly braces
 * and the return [2].
 *
 * [1]: https://en.wikipedia.org/wiki/Combinatory_logic
 * [2]: https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Functions/Arrow_functions
 */

const ID    = (x) => x;
const CONST = (x) => (y) => x;
const COMP  = (f, g) => (x) => f(g(x));





/** Test Utilities
 *
 * We need some functions to help us test out our combinators, we'll just import
 * them here.
 */

const { assert, stringify } = require("./assert.js");
const { log } = console;

{
    // Assert checks equality between potentially nested data structures.

    assert("Assert Numbers", 1, 1);
    assert("Assert Arrays", [2], [2]);
    assert("Assert Nested Arrays", [[1], 2], [[1], 2]);

    // Stringify is a bit more unusual, it is easiest to see what it does by
    // looking at the examples below.

    const f = stringify("f");
    const g = stringify("g");
    assert("Stringify f", "f(x)", f("x"));
    assert("Stringify compose", "f(g(x))", COMP(f, g)("x"))
}





/** Booleans
 *
 * There are two distinct booleans, we will call them `T` and `F`. What
 * properties do they need to have? They need to:
 *
 * - Be combinators.
 * - Have the same interface as each other. For combinators this (roughly) means
 *   that they need to accept the same number of parameters.
 * - Be different from each other.
 *
 * We will also make no assumptions about the parameters that they could be
 * applied to.
 */

const T = (t, f) => t;
const F = (t, f) => f;

function fromBool(bool) {
    if (bool) {
        return T;
    } else {
        return F;
    }
}

function toBool(bool_f) {
    return bool_f(true, false);
}

{
    // There are lots of ways to check that we have actually defined booleans
    // just by looking at the constraints we outlined earlier, and making sure
    // we have met them.
    //
    // We are going to take a shortcut, however, by defining a way to convert
    // between our combinators and the concepts they simulate. If converting a
    // regular boolean into a combinator and back preserves it (true comes back
    // out as true, and false back out as false), and conversely, converting a
    // combinator boolean into a regular boolean and back preserves it, we know
    // we're on to a good thing.
    //
    // We check one direction below, but the other direction is harder to test
    // reliably, because it involves checking equality between functions, so
    // you'll have to take my word for it!

    assert("Encode True",  true,  toBool(fromBool(true)));
    assert("Encode False", false, toBool(fromBool(false)));
}





/** If Expressions
 *
 * What use would booleans be if you couldn't condition on them? The definition
 * of `IF` is very simple, we take the condition `c` and use it to "pick" which
 * of the `t`hen or `e`lse cases to evaluate. If `c` evaluated to `T` then it
 * picks its first argument (`t`), and if it evaluates to `F`, it picks its
 * second argument (`f`).
 *
 * Note that `t` and `e` are 0-parameter combinators whose bodies contain the
 * expressions to evaluate in the then and else cases respectively. After the
 * appropriate case is picked, it is called to evaluate it.
 */

const IF  = (c, t, e) => c(t, e)();





/** Logical And */

const AND = (l, r) => IF(l, () => r, () => F);

{
    const [t, f] = [true, false].map(fromBool);

    assert("AND(F, F)", false, toBool(AND(f, f)));
    assert("AND(F, T)", false, toBool(AND(f, t)));
    assert("AND(T, F)", false, toBool(AND(t, f)));
    assert("AND(T, T)", true,  toBool(AND(t, t)));
}





/** Logical Or */

const OR = (l, r) => IF(l, () => T, () => r);

{
    const [t, f] = [true, false].map(fromBool);

    assert("OR(F, F)", false, toBool(OR(f, f)));
    assert("OR(F, T)", true,  toBool(OR(f, t)));
    assert("OR(T, F)", true,  toBool(OR(t, f)));
    assert("OR(T, T)", true,  toBool(OR(t, t)));
}






/** Logical Not */

const NOT = (b) => IF(b, () => F, () => T);

{
    const [t, f] = [true, false].map(fromBool);

    assert("NOT(T)", false, toBool(NOT(t)));
    assert("NOT(F)", true,  toBool(NOT(f)));
}






/** Natural Numbers
 *
 * The "Natural" numbers are just the non-negative integers (0, 1, 2,
 * 3,...). Like with booleans, we should choose a representation that we can
 * both encode and decode into JavaScript's own numbers.
 *
 * Our chosen encoding maps:
 *
 *   0 to (s, z) => z
 *   1 to (s, z) => s(z)
 *     ...
 *   5 to (s, z) => s(s(s(s(s(z)))))
 *
 * In other words, a number, `n`, is represented by taking a combinator `s` and
 * a parameter `z`, and applying `s` to `z`, `n` times.
 *
 * We begin by defining combinators to construct numbers with. It suffices to
 * define `Z`, our encoding of 0, and `S`, the "successor" combinator, which,
 * when given the encoding of a number, returns the encoding of the number one
 * greater.
 */

const Z = (s, z) => z;
const S = (n) => (s, z) => s(n(s, z));

function fromNum(num) {
    if (num === 0) {
        return Z;
    } else {
        return S(fromNum(num - 1));
    }
}

function toNum(num_f) {
    return num_f((n) => n + 1, 0);
}

{
    assert("Encode Zero",     0, toNum(fromNum(0)));
    assert("Encode Non-Zero", 5, toNum(fromNum(5)));
}





/** Is Zero?
 *
 * Suppose we wish to check whether a number is 0 or not. One way to look at
 * what we are doing is that we are consuming the number, to produce a boolean.
 * To consume a combinator-encoded number, we have to apply it, but the question
 * is, to what?
 *
 * Applying zero will always give you back the second parameter, so it is clear
 * that we must give `T` as our second parameter to satisfy `IS_ZERO(Z) = T`.
 *
 * Applying a non-zero number will give back the result of applying the first
 * parameter a number of times to `T`. No matter how many times we apply it, the
 * return value should be `F`, so the first parameter is `(_) => F`.
 */

const IS_ZERO = (n) => n((_) => F, T);

{
    assert("Test Zero",     true,  toBool(IS_ZERO(fromNum(0))));
    assert("Test Non-Zero", false, toBool(IS_ZERO(fromNum(3))));
}

/** Parity
 *
 * Checking whether a number is even (or odd, respectively) is another operation
 * that consumes numbers and produces booleans, so again, we have to choose
 * combinators to apply the number to.
 *
 * As before, by looking at zero we see that the second parameter has to be `T`
 * in the definition of `IS_EVEN` (because 0 is even) and must be `F` in
 * `IS_ODD`.
 *
 * The first parameter, `s`, is a bit more interesting. Let us look at some
 * examples:
 *
 *   IS_EVEN(one) ---> one(s, T) --->   s(T)  -?-> F
 *   IS_EVEN(two) ---> one(s, T) ---> s(s(T)) -?-> s(F) -?-> T
 *
 * So, `s(T)` should be `F` and `s(F)` should be `T`. `s` is `NOT`!
 */

const IS_EVEN = (n) => n(NOT, T);
const IS_ODD  = (n) => n(NOT, F);

{
    for (let i = 0; i < 4; ++i) {
        const ni = fromNum(i);
        assert(`${i} even parity`, i % 2 === 0, toBool(IS_EVEN(ni)));
        assert(`${i} odd parity`,  i % 2 === 1, toBool(IS_ODD(ni)));
    }
}


/** Addition
 *
 * What does it mean to add two combinator numbers, `m` and `n`?  Let us say
 * that `m` applies its first argument to its second `x` times and `n` applies
 * its first argument to its second `y` times. The result should be a combinator
 * number that accepts two arguments, `s` and `z`, and we need to apply `s` to
 * `z` `x + y` times.
 *
 * Rewording the problem, this is the same as applying `s` `x` times to the
 * result of applying `s` `y` times to `z`. Or, applying `s` `x` times to
 * `n(s, z)`. Or `m(s, n(s, z))`.
 */

const ADD  = (m, n) => (s, z) => m(s, n(s, z));

{
    // Addition also has a bunch of rules that it needs to satisfy. We're going
    // to avoid proving them ourselves again by using our encoding/decoding
    // trick. We expect that JavaScript's own addition satisfies the rules, so
    // all we need to show is that if we:
    //
    // - Encode two numbers: m, n
    // - Perform our addition
    // - Decode the result
    //
    // It should be equal to `m + n`. This should hold for any `m` and `n`, but
    // we only check some key cases below.

    const [n0, n2, n3, n4] = [0, 2, 3, 4].map(fromNum);

    assert("Add Left Identity",  4, toNum(ADD(n0, n4)));
    assert("Add Right Identity", 4, toNum(ADD(n4, n0)));
    assert("Add Arbitrary",      5, toNum(ADD(n2, n3)));
    assert("Add Symmetric",      5, toNum(ADD(n3, n2)));
}





/** Multiplication
 *
 * We are given two combinator numbers, `m` and `n`. Let's say (once again) that
 * `m` performs `x` applications, and `n` performs `y`. Then we need to produce
 * a combinator that performs `x*y` applications.
 *
 * This is the same as performing `x` applications of a combinator that performs
 * `y` applications, where we know how to perform `x` applications (using `m`),
 * and we know how to perform `y` applications (using `n`)!
 */

const MUL  = (m, n) => (s, z) => m((a) => n(s, a), z);

{
    const [n0, n1, n2, n3, n4] = [0, 1, 2, 3, 4].map(fromNum);

    assert("Mul Left Zero",      0, toNum(MUL(n0, n2)));
    assert("Mul Right Zero",     0, toNum(MUL(n2, n0)));
    assert("Mul Left Identity",  4, toNum(MUL(n1, n4)));
    assert("Mul Right Identity", 4, toNum(MUL(n4, n1)));
    assert("Mul Arbitrary",      6, toNum(MUL(n2, n3)));
    assert("Mul Symmetric",      6, toNum(MUL(n3, n2)));
}





/** Pairs
 *
 * Pairs store two pieces of information, a first and a second piece. All we
 * want from a pair is to be able to recover these pieces of information later.
 *
 * We represent them as higher-order functions: A pair holding `1` and `2` is
 * `(p) => p(1, 2)`, we can recover the first or second part by passing in an
 * appropriate combinator, `p`.
 *
 * Also note that we don't care what goes inside the pair, they could be
 * combinators, or they could be regular JavaScript objects, arrays, numbers,
 * etc.
 */

const PAIR = (fst, snd) => (p) => p(fst, snd);
const FST  = (p) => p((fst, snd) => fst);
const SND  = (p) => p((fst, snd) => snd);

function fromPair(pair) {
    return PAIR(pair[0], pair[1]);
}

function toPair(pair_f) {
    return [FST(pair_f), SND(pair_f)];
}

{
    const pair = fromPair([1, 2]);
    assert("Pair Encoding", [1, 2], toPair(pair));
    assert("Pair First",  1, FST(pair));
    assert("Pair Second", 2, SND(pair));
}





/** Optionals
 *
 * A datatype is only as good as the actions you can perform on it. This is why,
 * when we defined numbers, and pairs, we defined them as computations waiting
 * for functions. Pairs are two values waiting for a function that accepts two
 * parameters, numbers embody iteration.
 *
 * Mercifully, there is no `null` in our walled garden of combinators, but we do
 * sometimes need optional values.
 *
 * Whenever we consume an optional value, we supply both a combinator to apply,
 * and a default value. If the optional value is something, then the function
 * will be applied to it and the result returned, if it is nothing, then the
 * default value is returned.
 */

const SOME    = (v) => (s, n) => s(v)
const NOTHING = (s, n) => n

function fromOptional(opt) {
    if (opt === null) {
        return NOTHING;
    } else {
        return SOME(opt);
    }
}

function toOptional(opt_f) {
    return opt_f(ID, null);
}

{
    assert("Optional Null Encoding",     null, toOptional(fromOptional(null)));
    assert("Optional Non-Null Encoding", 1,    toOptional(fromOptional(1)));
}





/** Lists
 *
 * We'll jump straight to what our encoding looks like for lists.
 *
 *   []              becomes (c, n) => n
 *   [1]             becomes (c, n) => c(1, n)
 *   [1, 2]          becomes (c, n) => c(1, c(2, n))
 *                   ...
 *   [1, 2, 3, 4, 5] becomes (c, n) => c(1, c(2, c(3, c(4, c(5, n)))))
 *
 * Like our encoding of numbers, we build lists from the empty list, `NIL`, and
 * `CONS` which prefixes an element on to the front of an existing list. They
 * correspond to `Z` and `S` for numbers, respectively.
 */

const CONS  = (hd, tl) => (c, n) => c(hd, tl(c, n));
const NIL   = (c, n) => n;

function fromArray(array) {
    if (array.length === 0) {
        return NIL;
    } else {
        return CONS(array[0], fromArray(array.slice(1)));
    }
}

function toArray(list_f) {
    return list_f((hd, tl) => {
        tl.unshift(hd);
        return tl;
    }, []);
}

{
    assert("List Encoding", [1, 2, 3], toArray(fromArray([1, 2, 3])));
}





/** Head and Tail
 *
 * `HD` returns the first element of a list, and `TL` returns all but the first
 * element. Both these combinators only produce meaningful results when applied
 * to non-empty lists, but any list can be given to them. This is where our
 * optionals come in handy.
 *
 * The definition of `HD` comes easily enough: The head of an empty list is
 * `NOTHING`, the head of a non-empty list is, the first parameter passed to the
 * combinator that acts on non-empty list (wrapped in the `SOME` combinator.) In
 * a sense, this corresponds to `IS_ZERO` for numbers, and optionals correspond
 * to booleans.
 */

const HD = (l) => l((hd, _) => SOME(hd), NOTHING)

/**
 * In order to calculate the tail of a list, we define another function:
 * `SPLAT`. Splatting a non-empty list splits it apart at the top, into some
 * pair of head and tail. Splatting an empty list gives nothing. It is
 * straightforward to define `TL` using this.
 */

const SPLAT = (list) => {
    const WHEN_NOT_EMPTY = (hd, tailSplat) =>
          SOME(PAIR(hd, tailSplat((p) => p(CONS), NIL)));

    return list(WHEN_NOT_EMPTY, NOTHING);
}

const TL = (l) => SPLAT(l)((pair) => SOME(SND(pair)), NOTHING)

{
    assert("Empty Head",     null, toOptional(HD(NIL)));
    assert("Non-Empty Head", 1,    toOptional(HD(CONS(1, NIL))));
    assert("Empty Tail",     null, toOptional(TL(NIL)));

    assert("Non-Empty Tail", [2, 3],
           toArray(toOptional(TL(fromArray([1, 2, 3])))));
}





/** List Concatenation
 *
 * If viewed in the right light, list concatenation looks just like addition for
 * numbers...
 */

const CONCAT = (xs, ys) => (c, n) => xs(c, ys(c, n));

{
    const xs = fromArray([1, 2, 3]);
    const ys = fromArray([4, 5, 6]);

    assert("Concat Left Identity",  [4, 5, 6],     toArray(CONCAT(NIL, ys)));
    assert("Concat Right Identity", [1, 2, 3],     toArray(CONCAT(xs, NIL)));
    assert("Concat Arbitrary", [1, 2, 3, 4, 5, 6], toArray(CONCAT(xs, ys)));
}





/** List Length
 *
 * Calculating the length of a list can also be seen as converting a list to a
 * number. We do this by ignoring the elements of the list but keeping the
 * structure of the function applications. Consider the encodings of [1,2,3,4]
 * and the number 4:
 *
 * - (c, n) => c(1, c(2, c(3, c(4, n))))
 * - (s, z) => s(   s(   s(   s(   z))))
 *
 * respectively. When aligned in this way, numbers look very much like lists
 * with no space for elements.
 */

const LEN = (xs) => xs((_, tailLen) => S(tailLen), Z);

{
    assert("Length Empty List",     0, toNum(LEN(NIL)));
    assert("Length Non-Empty List", 3, toNum(LEN(fromArray([1,2,3]))));
}





/** Mapping
 *
 * Mapping is usually defined recursively: Mapping over an empty list yields the
 * empty list, and mapping over a non-empty list involves applying the function
 * to the head, mapping over the tail, and then combining the two results with
 * `CONS`.
 *
 * We have forbidden recursion, however, so "mapping over the tail" in this
 * sense, is not feasible. However, the first parameter to the list is a
 * combinator that is applied at every position in the list to the element at
 * that position as well as the result of the application before it. This
 * structure can be exploited to combine the elements of a list together, in
 * this case, back into a list, but with transformed elements.
 */

const MAP = (f, xs) => xs((hd, mappedTl) => CONS(f(hd), mappedTl), NIL)

{
    const list = fromArray([1, 2, 3].map(fromNum));
    assert("Map", [2, 3, 4], toArray(MAP(S, list)).map(toNum));
}





/** Filtering */

const FILTER = (p, xs) => xs((hd, filteredTl) =>
                             IF(p(hd),
                                () => CONS(hd, filteredTl),
                                () => filteredTl),
                             NIL);

{
    const list = fromArray([1, 0, 2, 0, 3, 0].map(fromNum));

    assert("Filter", [0, 0, 0],
           toArray(FILTER(IS_ZERO, list)).map(toNum));

    assert("Filter Complement", [1, 2, 3],
           toArray(FILTER(COMP(NOT, IS_ZERO), list)).map(toNum));
}





/** BONUS: Folding */

const FOLDR = (f, e, xs) => xs(f, e);

const FOLDL = (f, e, xs) => xs((hd, foldedTail) =>
                               (acc) => foldedTail(f(acc, hd)), ID)(e)

{
    const list = fromArray(["x1", "x2", "x3"]);

    assert("Right fold",
           "f(x1, f(x2, f(x3, e)))",
           FOLDR(stringify("f"), "e", list));

    assert("Left fold",
           "f(f(f(e, x1), x2), x3)",
           FOLDL(stringify("f"), "e", list));
}





/** BONUS: (Saturated) Subtraction */

const PRED = (n) => (f, x) => n((y) => (g) => g(y(f)), CONST(x))(ID);
const SUB  = (m, n) => n(PRED, m);

{
    const [n0, n2, n3, n4] = [0, 2, 3, 4].map(fromNum);

    assert("Sub Right Identity", 4, toNum(SUB(n4, n0)));
    assert("Sub Arbitrary",      1, toNum(SUB(n3, n2)));
    assert("Sub Saturation",     0, toNum(SUB(n2, n3)));
}





/** BONUS: Factorial */

const FACT = (m) => {
    const UPDATE_PAIR = (num, fact) =>
        PAIR(S(num), MUL(S(num), fact));

    return SND(m((prevPair) => prevPair(UPDATE_PAIR),
                 PAIR(Z, S(Z))));
}

{
    assert("0!", 1,   toNum(FACT(fromNum(0))));
    assert("1!", 1,   toNum(FACT(fromNum(1))));
    assert("5!", 120, toNum(FACT(fromNum(5))));
}





/** BONUS: Zipping Lists */

const ZIP = (xs, ys) => {
    const WHEN_NOT_EMPTY = (x, tailZipper) => (zs) => {
        const WHEN_SOME = (splatPair) =>
            splatPair((zhd, ztl) =>
                      CONS(PAIR(x, zhd),
                           tailZipper(ztl)));

        return SPLAT(zs)(WHEN_SOME, NIL);
    }

    return xs(WHEN_NOT_EMPTY, CONST(NIL))(ys);
}

{
    const long  = fromArray([1, 2, 3]);
    const short = fromArray([1, 2]);

    assert("Zip", [[1,1], [2,2], [3,3]],
           toArray(ZIP(long, long)).map(toPair));

    assert("Zip Short Left",  [[1,1], [2,2]],
           toArray(ZIP(short, long)).map(toPair));

    assert("Zip Short Right", [[1,1], [2,2]],
           toArray(ZIP(long, short)).map(toPair));
}
