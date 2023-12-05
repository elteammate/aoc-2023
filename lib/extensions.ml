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

    let fold_right1 f l = let k = Array.length l - 1 in Array.fold_right f (Array.sub l 0 k) l.(k)
    let fold_left1 f l = let k = Array.length l - 1 in Array.fold_left f l.(0) (Array.sub l 1 k)
end

module List = struct
    include List
    let flat_map f l = List.flatten @@ List.map f l

    let fold_left1 f l = Array.fold_left1 f @@ Array.of_list l
    let fold_right1 f l = Array.fold_right1 f @@ Array.of_list l
end

module String = struct 
    include String

    let of_char = String.make 1

    let sub' a start len = 
        let end_ = start + len in
        let start' = max start 0 in
        let end' = min (String.length a) end_ in
        let len' = end' - start' in 
            String.sub a start' len'

    let to_array s = Array.init (String.length s) (String.get s)

    let split_on_char' c s = String.split_on_char c s |> List.filter ((<>) "")

    let fold_lefti f a s = snd @@ String.fold_left (fun (i, a) s -> i + 1, f i a s) (0, a) s
end

let int_of_bool b = match b with
    | false -> 0
    | true -> 1

let rec (--) i j =
    if i >= j then 
        []
    else
        i :: ((i + 1) -- j)

let and_ () x = x

