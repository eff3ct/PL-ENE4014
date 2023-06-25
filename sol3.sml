datatype pattern = Wildcard 
    | Variable of string 
    | UnitP 
    | ConstP of int 
    | TupleP of pattern list
    | ConstructorP of string * pattern

datatype valu = Const of int
    | Unit
    | Tuple of valu list
    | Constructor of string * valu

fun fold f acc xs =
    case xs of
        [] => acc
        | x::xs' => fold f (f(x, acc)) xs'

fun check_pat(p: pattern): bool =
    let
        fun helper1(p: pattern, xs: string list) =
            case p of
                Wildcard => xs
                | Variable(x) => x::xs
                | UnitP => xs
                | ConstP(n) => xs
                | TupleP(p') => fold helper1 xs p'
                | ConstructorP(x, y) => helper1(y, xs) (* x is not a variable *)
        fun helper2(xs: string list) =
            case xs of
                [] => true
                | x::xs' => if List.exists (fn u => u = x) xs'
                    then false 
                    else helper2(xs')
    in
        helper2(helper1(p, []))
    end

fun match(v: valu, p: pattern): (string * valu) list option =
    case (v, p) of
        (Const(a), ConstP(b)) => if a = b
            then SOME []
            else NONE
        | (Unit, UnitP) => SOME []
        | (Tuple(a), TupleP(b)) =>
            if List.length(a) = List.length(b)
            then
                let
                    fun helper(xs: valu list, ys: pattern list, acc: (string * valu) list) =
                        case (xs, ys) of
                            ([], []) => SOME acc
                            | (x::xs', y::ys') => (
                                case match(x, y) of
                                    NONE => NONE
                                    | SOME zs => helper(xs', ys', acc @ zs))
                            | _ => NONE
                in
                    helper(a, b, [])
                end
            else NONE
        | (Constructor(a, x), ConstructorP(b, y)) => if a = b
            then match(x, y)
            else NONE
        | (_, Wildcard) => SOME []
        | (_, Variable(x)) => SOME [(x, v)]
        | _ => NONE

type name = string
datatype RSP =
    ROCK
    | SCISSORS
    | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament =
    PLAYER of name * (RSP strategy ref)
    | MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two, three, one))
val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)
fun next(strategyRef) = 
    let 
        val Cons(rsp, func) = !strategyRef
    in
        strategyRef := func(); rsp
    end 

fun whosWinner(t) =
    let
        fun helper(x, y) =
            let 
                val PLAYER(_, f1) = x
                val PLAYER(_, f2) = y
            in
                case (next(f1), next(f2)) of
                    (ROCK, SCISSORS) => x
                    | (SCISSORS, PAPER) => x
                    | (PAPER, ROCK) => x
                    | (SCISSORS, ROCK) => y
                    | (PAPER, SCISSORS) => y
                    | (ROCK, PAPER) => y
                    | _ => helper(x, y)
            end
    in
        case t of
            PLAYER(_, _) => t
            | MATCH(x, y) => helper(whosWinner(x), whosWinner(y))
    end
