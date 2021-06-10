datatype either a b = Left of a | Right of b

fun partitionEithers [a] [b] (es : list (either a b)) : (list a * list b) =
    let fun go [a] [b] (l : list a) (r : list b)
               (es : list (either a b)) : (list a * list b) = case es of
            | [] => (List.rev l, List.rev r)
            | ((Left l') :: es') => go (l' :: l) r es'
            | ((Right r') :: es') => go l (r' :: r) es'
    in
        go [] [] es
    end
