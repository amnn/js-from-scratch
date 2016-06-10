/**
 * JavaScript from Scratch
 *
 *   -- Ashok Menon (@amnn)
 */

"use strict";

/** Table of Contents [toc]
 *
 * ( ) Combinators [cmbntrs]
 * ^ ^ ^ ^ ^
 * | | | | |
 * | | | | ( ) Booleans [blns]
 * | | | | ^ ^
 * | | | | | |
 * | | | | | ( ) If Expressions [ifxprs]
 * | | | | | ^ ^ ^
 * | | | | | | | |
 * | | | | | | | ( ) And [land]
 * | | | | | | ` ( ) Or  [lor]
 * | | | | | `-- ( ) Not [lnot]
 * | | | | |     ^
 * | | | | | ,---'
 * | | | `-+-+ ( ) Natural Numbers [ntnmbrs]
 * | | |   | | ^ ^ ^ ^ ^
 * | | |   | | | | | | |
 * | | |   | `-+-+-+-+ ( ) Parity         [prty]
 * | | |   `---+-+-+-` ( ) Is Zero?       [szr]
 * | | |       | | `-- ( ) Addition       [addtn]
 * | | |       | `---- ( ) Subtraction    [sbtrctn]  ****
 * | | |       `------ ( ) Multiplication [mltplctn]
 * | | |               ^
 * | | | ,-------------'
 * | | `-+ ( ) Pairs [prs]
 * | |   | ^ ^
 * | |   | | |
 * | |   `-+ ( ) Factorial [fctrl] **
 * | | ,---'
 * | `-+-- ( ) Optionals [optnls]
 * | ,-'   ^
 * | | ,---'
 * `-+-+-- ( ) Lists [lsts]
 *   | |   ^ ^ ^ ^ ^
 *   | |   | | | | |
 *   | |   | | | | ( ) Concatenation [cnct]
 *   | |   | | | ` ( ) Length        [lngth]
 *   | |   | | `-- ( ) Map / Filter  [mpfltr]
 *   | |   | `---- ( ) Folding       [fldng]  ****
 *   `-`---`------ ( ) Head / Tail   [hdtl]   ***
 *                 ^
 *                 |
 *                 ( ) Zip [zppng] ****
 */





/** Test Utilities [tst] */

const { assert, stringify } = require("./assert.js");
const { log } = console;





/** Combinators [cmbntrs] */

const ID    = undefined;
const CONST = undefined;
const COMP  = undefined;

/*
{
  assert("Assert Numbers", 1, 1);
  assert("Assert Arrays", [2], [2]);
  assert("Assert Nested Arrays", [[1], 2], [[1], 2]);

  const f = stringify("f");
  const g = stringify("g");
  assert("Stringify f", "f(x)", f("x"));
  assert("Stringify compose", "f(g(x))", COMP(f, g)("x"));
}
 */





/** Booleans [blns] */

const T = undefined;
const F = undefined;

function fromBool(bool) {
  if (bool) {
    return T;
  } else {
    return F;
  }
}

function toBool(bool_f) {}

/*
{
  assert("Encode True",  true,  toBool(fromBool(true)));
  assert("Encode False", false, toBool(fromBool(false)));
}
 */





/** If Expressions [ifxprs] */

const IF = undefined;





/** Logical And [land] */

const AND = undefined;

/*
{
  const [t, f] = [true, false].map(fromBool);

  assert("AND(F, F)", false, toBool(AND(f, f)));
  assert("AND(F, T)", false, toBool(AND(f, t)));
  assert("AND(T, F)", false, toBool(AND(t, f)));
  assert("AND(T, T)", true,  toBool(AND(t, t)));
}
 */





/** Logical Or [lor] */

const OR = undefined;

/*
{
  const [t, f] = [true, false].map(fromBool);

  assert("OR(F, F)", false, toBool(OR(f, f)));
  assert("OR(F, T)", true,  toBool(OR(f, t)));
  assert("OR(T, F)", true,  toBool(OR(t, f)));
  assert("OR(T, T)", true,  toBool(OR(t, t)));
}
 */






/** Logical Not [lnot] */

const NOT = undefined;

/*
{
  const [t, f] = [true, false].map(fromBool);

  assert("NOT(T)", false, toBool(NOT(t)));
  assert("NOT(F)", true,  toBool(NOT(f)));
}
 */






/** Natural Numbers [ntnmbrs] */

const Z = undefined;
const S = undefined;

function fromNum(num) {
  if (num === 0) {
    return Z;
  } else {
    return S(fromNum(num - 1));
  }
}

function toNum(num_f) {}

/*
{
  assert("Encode Zero",     0, toNum(fromNum(0)));
  assert("Encode Non-Zero", 5, toNum(fromNum(5)));
}
 */





/** Parity [prty] */

const IS_EVEN = undefined;
const IS_ODD  = undefined;

/*
{
  for (let i = 0; i < 4; ++i) {
    const ni = fromNum(i);
    assert(`${i} even parity`, i % 2 === 0, toBool(IS_EVEN(ni)));
    assert(`${i} odd parity`,  i % 2 === 1, toBool(IS_ODD(ni)));
  }
}
 */





/** Is Zero? [szr] */

const IS_ZERO = undefined;

/*
{
  assert("Test Zero",     true,  toBool(IS_ZERO(fromNum(0))));
  assert("Test Non-Zero", false, toBool(IS_ZERO(fromNum(3))));
}
 */





/** Addition [addtn] */

const ADD = undefined;

/*
{
  const [n0, n2, n3, n4] = [0, 2, 3, 4].map(fromNum);

  assert("Add Left Identity",  4, toNum(ADD(n0, n4)));
  assert("Add Right Identity", 4, toNum(ADD(n4, n0)));
  assert("Add Arbitrary",      5, toNum(ADD(n2, n3)));
  assert("Add Symmetric",      5, toNum(ADD(n3, n2)));
}
 */





/** (Saturated) Subtraction [sbtrctn] **** */

const PRED = undefined;
const SUB  = undefined;

/*
{
  const [n0, n2, n3, n4] = [0, 2, 3, 4].map(fromNum);

  assert("Sub Right Identity", 4, toNum(SUB(n4, n0)));
  assert("Sub Arbitrary",      1, toNum(SUB(n3, n2)));
  assert("Sub Saturation",     0, toNum(SUB(n2, n3)));
}
 */





/** Multiplication [mltplctn] */

const MUL = undefined;

/*
{
  const [n0, n1, n2, n3, n4] = [0, 1, 2, 3, 4].map(fromNum);

  assert("Mul Left Zero",      0, toNum(MUL(n0, n2)));
  assert("Mul Right Zero",     0, toNum(MUL(n2, n0)));
  assert("Mul Left Identity",  4, toNum(MUL(n1, n4)));
  assert("Mul Right Identity", 4, toNum(MUL(n4, n1)));
  assert("Mul Arbitrary",      6, toNum(MUL(n2, n3)));
  assert("Mul Symmetric",      6, toNum(MUL(n3, n2)));
}
 */





/** Pairs [prs] */

const PAIR = undefined;
const FST  = undefined;
const SND  = undefined;

function fromPair(pair) {
  return PAIR(pair[0], pair[1]);
}

function toPair(pair_f) {}

/*
{
  const pair = fromPair([1, 2]);
  assert("Pair Encoding", [1, 2], toPair(pair));
  assert("Pair First",  1, FST(pair));
  assert("Pair Second", 2, SND(pair));
}
 */





/** Factorial [fctrl] ** */

const FACT = undefined;

/*
{
  assert("0!", 1,   toNum(FACT(fromNum(0))));
  assert("1!", 1,   toNum(FACT(fromNum(1))));
  assert("5!", 120, toNum(FACT(fromNum(5))));
}
 */





/** Optionals [optnls] */

const SOME    = undefined;
const NOTHING = undefined;

function fromOptional(opt) {
  if (opt === null) {
    return NOTHING;
  } else {
    return SOME(opt);
  }
}

function toOptional(opt_f) {}

/*
{
  assert("Optional Null Encoding",     null, toOptional(fromOptional(null)));
  assert("Optional Non-Null Encoding", 1,    toOptional(fromOptional(1)));
}
 */





/** Lists [lsts] */

const CONS = undefined;
const NIL  = undefined;

function fromArray(array) {
  if (array.length === 0) {
    return NIL;
  } else {
    return CONS(array[0], fromArray(array.slice(1)));
  }
}

function toArray(list_f) {}

/*
{
  assert("List Encoding", [1, 2, 3], toArray(fromArray([1, 2, 3])));
}
 */





/** List Concatenation [cnct] */

const CONCAT = undefined;

/*
{
  const xs = fromArray([1, 2, 3]);
  const ys = fromArray([4, 5, 6]);

  assert("Concat Left Identity",  [4, 5, 6],     toArray(CONCAT(NIL, ys)));
  assert("Concat Right Identity", [1, 2, 3],     toArray(CONCAT(xs, NIL)));
  assert("Concat Arbitrary", [1, 2, 3, 4, 5, 6], toArray(CONCAT(xs, ys)));
}
 */





/** List Length [lngth] */

const LEN = undefined;

/*
{
  assert("Length Empty List",     0, toNum(LEN(NIL)));
  assert("Length Non-Empty List", 3, toNum(LEN(fromArray([1,2,3]))));
}
 */





/** Mapping and Filtering [mpfltr] */

const MAP = undefined;

const FILTER = undefined;

/*
{
  const list = fromArray([1, 0, 2, 0, 3, 0].map(fromNum));

  assert("Map", [2, 1, 3, 1, 4, 1],
         toArray(MAP(S, list)).map(toNum));

  assert("Filter", [0, 0, 0],
         toArray(FILTER(IS_ZERO, list)).map(toNum));

  assert("Filter Complement", [1, 2, 3],
         toArray(FILTER(COMP(NOT, IS_ZERO), list)).map(toNum));
}
 */





/** Folding [fldng] **** */

const FOLDR = undefined;

const FOLDL = undefined;

/*
{
  const list = fromArray(["x1", "x2", "x3"]);

  assert("Right fold",
         "f(x1, f(x2, f(x3, e)))",
         FOLDR(stringify("f"), "e", list));

  assert("Left fold",
         "f(f(f(e, x1), x2), x3)",
         FOLDL(stringify("f"), "e", list));
}
 */





/** Head and Tail [hdtl] *** */

const HD = undefined;

const SPLAT = undefined;

const TL = undefined;

/*
{
  assert("Empty Head",     null, toOptional(HD(NIL)));
  assert("Non-Empty Head", 1,    toOptional(HD(CONS(1, NIL))));
  assert("Empty Tail",     null, toOptional(TL(NIL)));

  assert("Non-Empty Tail", [2, 3],
         toArray(toOptional(TL(fromArray([1, 2, 3])))));
}
 */





/** Zipping Lists [zppng] **** */

const ZIP = undefined;

/*
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
 */
