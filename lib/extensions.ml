module List = struct
    include List
    let flat_map f l = List.flatten @@ List.map f l
end

module String = struct 
    include String
    let fold_lefti f a s = snd @@ String.fold_left (fun (i, a) s -> i + 1, f i a s) (0, a) s
    let of_char = String.make 1

    let sub' a start len = 
        let end_ = start + len in
        let start' = max start 0 in
        let end' = min (String.length a) end_ in
        let len' = end' - start' in 
            String.sub a start' len'

    let to_array s = Array.init (String.length s) (String.get s)
end

module Array = struct
    include Array
    let sub' a start len = 
        let end_ = start + len in
        let start' = max start 0 in
        let end' = min (Array.length a) end_ in
        let len' = end' - start' in 
            Array.sub a start' len'

    let maybe_get a i =
        if i >= 0 && i < Array.length a then
            Some a.(i)
        else
            None
end

let int_of_bool b = match b with
    | false -> 0
    | true -> 1

let rec (--) i j =
    if i >= j then 
        []
    else
        i :: ((i + 1) -- j)
