fun merge(a: int list, b: int list) =
    if null(a) then b
    else if null(b) then a
    else if hd(a) < hd(b)
    then hd(a) :: merge(tl(a), b)
    else hd(b) :: merge(a, tl(b))

fun reverse(a: int list) =
    let fun rev_(b: int list, tmp: int list) = 
            if null(b) then tmp
            else rev_(tl(b), hd(b) :: tmp)
    in
        rev_(a, [])
    end

fun pi(a: int, b: int, f: (int -> int)) =
    if a > b then 1
    else f(a) * pi(a + 1, b, f)

fun digits(a: int) =
    let fun get_digits(a: int) = 
            if a < 10 then [a]
            else (a mod 10) :: get_digits(a div 10)
    in
        reverse(get_digits(a))
    end

fun additivePersistence(a: int) =
    let fun count(a: int, c: int) =
            let fun sum(a: int list) =
                    if null(a) then 0
                    else hd(a) + sum(tl(a))
            in
                if a < 10 then c
                else count(sum(digits(a)), c + 1)
            end
    in
        count(a, 0)
    end

fun digitalRoot(a: int) =
    let fun root(a: int) =
            let fun sum(a: int list) =
                    if null(a) then 0
                    else hd(a) + sum(tl(a))
            in
                if a < 10 then a
                else root(sum(digits(a)))
            end
    in
        root(a)
    end
    