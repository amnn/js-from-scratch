// TODO: more comments.

/** Combinators */

const ID    = (x) => x;
const CONST = (x) => (y) => x;
const COMP  = (f, g) => (x) => f(g(x))

/** Assertion function */

function assert(msg, expected, actual) {
    function eq(xs, ys) {
        // Check for nulls
        if (xs === null && ys === null) {
            return true;
        } else if (xs === null || ys == null) {
            return false;
        }

        // Recurse into arrays
        if (typeof xs === 'object' && typeof ys === 'object') {
            const xsIsArray = xs.constructor === Array;
            const ysIsArray = ys.constructor === Array;

            if (xsIsArray && ysIsArray) {
                return xs.every((x, i) => eq(x, ys[i]));
            } else if (xsIsArray || ysIsArray) {
                return false;
            }
        }

        // Catch all case
        return xs === ys;
    }

    if (eq(expected, actual)) {
        console.log("\u2714", msg);
    } else {
        console.log("\u2717", msg);
        console.log("  Expected:", expected);
        console.log("  Actual:  ", actual);
    }
}

/*
assert("Assert Numbers", 1, 1);
assert("Assert Arrays", [2], [2]);
assert("Assert Nested Arrays", [[1], 2], [[1], 2]);
*/

/** Booleans */

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

/*
assert("Encode True",  true,  toBool(fromBool(true)));
assert("Encode False", false, toBool(fromBool(false)));
*/

const IF  = (c, t, e) => c(t, e)();
const AND = (l, r) => IF(l, () => r, () => F);
const OR  = (l, r) => IF(l, () => T, () => r);
const NOT = (b)    => IF(b, () => F, () => T);

/*
assert("AND(F, F)", false, toBool(AND(fromBool(false), fromBool(false))));
assert("AND(F, T)", false, toBool(AND(fromBool(false), fromBool(true))));
assert("AND(T, F)", false, toBool(AND(fromBool(true),  fromBool(false))));
assert("AND(T, T)", true,  toBool(AND(fromBool(true),  fromBool(true))));
*/

/*
assert("OR(F, F)", false, toBool(OR(fromBool(false), fromBool(false))));
assert("OR(F, T)", true,  toBool(OR(fromBool(false), fromBool(true))));
assert("OR(T, F)", true,  toBool(OR(fromBool(true),  fromBool(false))));
assert("OR(T, T)", true,  toBool(OR(fromBool(true),  fromBool(true))));
*/

/*
assert("NOT(T)", false, toBool(NOT(fromBool(true))));
assert("NOT(F)", true,  toBool(NOT(fromBool(false))));
*/

/** Numbers */

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

/*
assert("Encode Zero",     0, toNum(fromNum(0)));
assert("Encode Non-Zero", 5, toNum(fromNum(5)));
*/

const IS_ZERO = (n) => n((_) => F, T);

/*
assert("Test Zero",     true,  toBool(IS_ZERO(fromNum(0))));
assert("Test Non-Zero", false, toBool(IS_ZERO(fromNum(3))));
*/

const ADD  = (m, n) => (f, x) => m(f, n(f, x));

/*
assert("Add Left  Identity", 4, toNum(ADD(fromNum(0), fromNum(4))));
assert("Add Right Identity", 4, toNum(ADD(fromNum(4), fromNum(0))));
assert("Add Arbitrary",      5, toNum(ADD(fromNum(2), fromNum(3))));
assert("Add Symmetric",      5, toNum(ADD(fromNum(3), fromNum(2))));
*/

const MUL  = (m, n) => (f, x) => m((y) => n(f, y), x);

/*
assert("Mul Left  Zero",     0, toNum(MUL(fromNum(0), fromNum(2))));
assert("Mul Right Zero",     0, toNum(MUL(fromNum(2), fromNum(0))));
assert("Mul Left  Identity", 4, toNum(MUL(fromNum(1), fromNum(4))));
assert("Mul Right Identity", 4, toNum(MUL(fromNum(4), fromNum(1))));
assert("Mul Arbitrary",      6, toNum(MUL(fromNum(2), fromNum(3))));
assert("Mul Symmetric",      6, toNum(MUL(fromNum(3), fromNum(2))));
*/

const PRED = (n) => (f, x) => n((y) => (g) => g(y(f)), CONST(x))(ID);
const SUB  = (m, n) => n(PRED, m);

/*
assert("Sub Right Identity", 4, toNum(SUB(fromNum(4), fromNum(0))));
assert("Sub Arbitrary",      1, toNum(SUB(fromNum(3), fromNum(2))));
assert("Sub Absorption",     0, toNum(SUB(fromNum(2), fromNum(3))));
*/

/** Pairs */

const PAIR = (fst, snd) => (p) => p(fst, snd);
const FST  = (p) => p((fst, snd) => fst);
const SND  = (p) => p((fst, snd) => snd);

function fromPair(pair) {
    return PAIR(pair[0], pair[1]);
}

function toPair(pair) {
    return [FST(pair), SND(pair)];
}

/*
assert("Pair Encoding", [1, 2], toPair(fromPair([1, 2])));
assert("Pair First",  1, FST(fromPair([1, 2])));
assert("Pair Second", 2, SND(fromPair([1, 2])));
*/

/** Optional */

const JUST    = (x) => (j, n) => j(x)
const NOTHING = (j, n) => n

function fromOptional(opt) {
    if (opt === null) {
        return NOTHING;
    } else {
        return JUST(opt);
    }
}

function toOptional(opt_f) {
    return opt_f(ID, null);
}

/*
assert("Optional Null     Encoding", null, toOptional(fromOptional(null)));
assert("Optional Non-Null Encoding", 1,    toOptional(fromOptional(1)));
*/

/** Lists */

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

/*
assert("List Encoding", [1, 2, 3], [1, 2, 3]);
*/

const SPLAT = (list) => {
    const WHEN_JUST = (splatPair) =>
          CONS(FST(splatPair), SND(splatPair));

    const WHEN_NOT_EMPTY = (hd, tailSplat) =>
          JUST(PAIR(hd, tailSplat(WHEN_JUST, NIL)));

    return list(WHEN_NOT_EMPTY, NOTHING);
}


const HD = (l) => l((hd, _) => JUST(hd), NOTHING)
const TL = (l) => SPLAT(l)((pair) => JUST(SND(pair)), NOTHING)

/*
assert("Empty Head",     null,   toOptional(HD(NIL)));
assert("Non-Empty Head", 1,      toOptional(HD(CONS(1, NIL))));
assert("Empty Tail",     null,   toOptional(TL(NIL)));
assert("Non-Empty Tail", [2, 3], toArray(toOptional(TL(fromArray([1, 2, 3])))));
*/

const CONCAT = (xs, ys) => (c, n) => xs(c, ys(c, n));

/*
assert("Concat Left Identity",
       [1, 2, 3],
       toArray(CONCAT(NIL, fromArray([1,2,3]))));
assert("Concat Right Identity",
       [1, 2, 3],
       toArray(CONCAT(fromArray([1,2,3]), NIL)));
assert("Concat Arbitrary",
       [1, 2, 3, 4, 5, 6],
       toArray(CONCAT(fromArray([1,2,3]), fromArray([4, 5, 6]))));
*/

const LEN = (xs) => xs((_, tailLen) => S(tailLen), Z);

/*
assert("Length Empty     List", 0, toNum(LEN(NIL)));
assert("Length Non-Empty List", 3, toNum(LEN(fromArray([1,2,3]))));
*/

const MAP = (f, xs) => xs((hd, mappedTl) => CONS(f(hd), mappedTl), NIL)

/*
assert("Map", [2, 3, 4],
       toArray(MAP(S, fromArray([1, 2, 3].map(fromNum)))).map(toNum));
*/

const FILTER = (p, xs) => xs((hd, filteredTl) =>
                             IF(p(hd),
                                () => CONS(hd, filteredTl),
                                () => filteredTl),
                             NIL);

/*
assert("Filter", [0, 0, 0],
       toArray(FILTER(IS_ZERO,
                      fromArray([1, 0, 2, 0, 3, 0]
                                  .map(fromNum))))
         .map(toNum));

assert("Filter Complement", [1, 2, 3],
       toArray(FILTER(COMP(NOT, IS_ZERO),
                      fromArray([1, 0, 2, 0, 3]
                                  .map(fromNum))))
         .map(toNum));
*/

const FOLDR = (f, e, xs) => xs(f, e);

const FOLDL = (f, e, xs) => xs((hd, foldedTail) =>
                               (acc) => foldedTail(f(acc, hd)), ID)(e)

/*
const stringify = (x, y) => "f(" + x + ", " + y + ")";
assert("Right fold",
       "f(x1, f(x2, f(x3, e)))",
       FOLDR(stringify, "e",
             fromArray(["x1", "x2", "x3"])));

assert("Left fold",
       "f(f(f(e, x1), x2), x3)",
       FOLDL(stringify, "e",
             fromArray(["x1", "x2", "x3"])));
*/

const FACT = (m) => {
    const UPDATE_PAIR = (num, fact) =>
        PAIR(S(num), MUL(S(num), fact));

    return SND(m((prevPair) => prevPair(UPDATE_PAIR),
                 PAIR(Z, S(Z))));
}

/*
assert("0!", 1,   toNum(FACT(fromNum(0))));
assert("1!", 1,   toNum(FACT(fromNum(1))));
assert("5!", 120, toNum(FACT(fromNum(5))));
*/

const ZIP = (xs, ys) => {
    const WHEN_NOT_EMPTY = (x, tailZipper) => (zs) => {
        const WHEN_JUST = (splatPair) =>
            splatPair((zhd, ztl) =>
                      CONS(PAIR(x, zhd),
                           tailZipper(ztl)));

        return SPLAT(zs)(WHEN_JUST, NIL);
    }

    return xs(WHEN_NOT_EMPTY, CONST(NIL))(ys);
}

/*
assert("Zip", [[1,1], [2,2], [3,3]],
       toArray(ZIP(fromArray([1,2,3]), fromArray([1,2,3]))).map(toPair));
assert("Zip Short Left",  [[1,1], [2,2]],
       toArray(ZIP(fromArray([1,2]), fromArray([1,2,3]))).map(toPair));
assert("Zip Short Right", [[1,1], [2,2]],
       toArray(ZIP(fromArray([1,2,3]), fromArray([1,2]))).map(toPair));
*/
