[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-33"]
[@@@warning "-34"]
[@@@warning "-37"]
[@@@warning "-69"]

open Shared 
open Extensions

let input = Input.require_input_file ()

let split_at i s = String.sub s 0 i, String.sub s i (String.length s - i)

let stripl p s = 
    match split_at (String.length p) s with
    | pref, rest when pref = p -> Some rest
    | _ | exception _ -> None

let stripr p s =
    match split_at (String.length s - String.length p) s with
    | rest, suff when suff = p -> Some rest
    | _ | exception _ -> None

let stripl_or_panic p s = 
    match stripl p s with
    | Some res -> res
    | _ -> failwith @@ Format.sprintf "Failed to match start of the string %s with string %s" s p

let stripr_or_panic p s = 
    match stripr p s with
    | Some res -> res
    | _ -> failwith @@ Format.sprintf "Failed to match end of the string %s with string %s" s p

let start_seeds = input_line input |> stripl_or_panic "seeds: " |> String.split_on_char ' ' |> List.map int_of_string |> Array.of_list
let start_seeds = snd @@ Array.fold_left (fun (el, pairs) x -> 
    match el with 
    | None -> Some x, pairs
    | Some y -> None, (y, x) :: pairs
) (None, []) start_seeds

let () = Batteries.dump start_seeds |> print_endline
let () = input_line input |> ignore
let () = input_line input |> ignore

let rec parse_input input = match String.trim @@ input_line input with
    | "" -> parse_input input
    | s when s.[String.length s - 1] = ':' -> [] :: parse_input input
    | s -> begin match String.split_on_char ' ' s, parse_input input with
        | [a; b; c], top :: rest -> ((int_of_string a, int_of_string b, int_of_string c) :: top) :: rest
        | _ -> failwith "parsing error"
    end
    | exception End_of_file -> [[]] 

let input = parse_input input
let () = Batteries.dump @@ input |> print_endline

let rec solve input (l, k) = begin
    match input with
    | [] -> l
    | [] :: next -> solve next (l, k)
    | ((t, s, n) :: rest) :: next when l + k <= s -> solve (rest::next) (l, k)
    | ((t, s, n) :: rest) :: next when l >= s + n -> solve (rest::next) (l, k)
    | ((t, s, n) :: rest) :: next when l >= s && l + k <= s + n -> solve next (t + l - s, k)
    | ((t, s, n) :: rest) :: next when l < s && l + k <= s + n -> min (solve (rest::next) (l, (s - l))) (solve next (t, (l + k - s)))
    | ((t, s, n) :: rest) :: next when l >= s && l + k > s + n -> min (solve (next) (t + (l - s), s + n - l)) (solve (rest::next) (s + n, l + k - s - n))
    | ((t, s, n) :: rest) :: next -> min (solve (rest::next) (l, s - l)) @@ min (solve next (t, n)) (solve (rest::next) (s + n, l + k - s - n))
    end

let () = List.fold_right1 min @@ List.map (solve input) start_seeds |> print_int

