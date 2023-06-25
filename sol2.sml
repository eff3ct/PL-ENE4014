datatype expr = NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr

datatype formula = TRUE
    | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr

fun eval(f) =
    let
        fun calc(expr) =
            case expr of
                NUM(x) => x
                | PLUS(x1, x2) => calc(x1) + calc(x2)
                | MINUS(x1, x2) => calc(x1) - calc(x2)
    in
        case f of 
            TRUE => true
            | FALSE => false
            | NOT(x) => not(eval(x))
            | ANDALSO(x1, x2) => eval(x1) andalso eval(x2)
            | ORELSE(x1, x2) => eval(x1) orelse eval(x2)
            | IMPLY(x1, x2) => not(eval(x1)) orelse eval(x2)
            | LESS(x1, x2) => calc(x1) < calc(x2)
    end

type name = string
datatype metro = STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro

fun checkMetro(m: metro) =
    let
        fun existMetro(a: metro, b: string, n: int, bound: int) =
            if bound <= n
            then false
            else
                case a of
                    STATION(t) => false
                    | AREA(t1, t2) => t1 = b orelse existMetro(t2, b, n + 1, bound)
                    | CONNECT(t1, t2) => existMetro(t1, b, n + 1, bound) orelse existMetro(t2, b, n + 1, bound)
    in
        let
            fun validateMetro(u: metro, n: int) =
                case u of
                    STATION(t) => existMetro(m, t, 0, n)
                    | AREA(t1, t2) => validateMetro(t2, n + 1)
                    | CONNECT(t1, t2) => validateMetro(t1, n + 1) andalso validateMetro(t2, n + 1)
        in
            validateMetro(m, 0)
        end
    end

datatype 'a lazyList = nullList
    | cons of 'a * (unit -> 'a lazyList)

fun seq(first: int, last: int) =
    if first <= last
    then
        let
            fun tail() =
                seq(first + 1, last)
        in
            cons(first, tail)
        end
    else nullList

fun infSeq(first: int) = 
    let 
        fun tail() =
            infSeq(first + 1)
    in
        cons(first, tail)
    end

fun firstN(lazyListVal: 'a lazyList, n: int) =
    if n <= 0
    then []
    else 
        case lazyListVal of
            cons(x, y) => x :: firstN(y(), n - 1)
            | nullList => []

fun Nth(lazyListVal: 'a lazyList, n: int) =
    if n <= 0
    then NONE
    else
        case lazyListVal of
            nullList => NONE
            | cons(x, y) => if n = 1 
                            then SOME x 
                            else Nth(y(), n - 1)

fun filterMultiples(lazyListVal: int lazyList, n: int) = 
    case lazyListVal of
        nullList => nullList
        | cons(x, y) => if x mod n = 0
                        then
                            filterMultiples(y(), n)
                        else
                            let
                                fun tail() =
                                    filterMultiples(y(), n)
                            in
                                cons(x, tail)
                            end
                                
fun primes() = 
    let
        fun seive(lazyListVal: int lazyList) =
            case lazyListVal of
                nullList => nullList
                | cons(x, y) => let
                                    fun tail() =
                                        seive(filterMultiples(lazyListVal, x))
                                in
                                    cons(x, tail)
                                end
    in
        seive(infSeq(2))
    end
                                
