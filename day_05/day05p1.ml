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

let rec solve seeds mapped input = 
    match String.trim @@ input_line input with
        | "" -> solve mapped (Array.copy mapped) input
        | s when s.[String.length s - 1] = ':' -> solve seeds mapped input
        | s -> begin match String.split_on_char ' ' s with
            | [a; b; c] -> begin 
                let s, l, k = (int_of_string a, int_of_string b, int_of_string c) in
                    Array.mapi (fun i x -> 
                        if x >= l && x < l + k then
                            mapped.(i) <- s + (x - l)
                        else
                            ()
                    ) seeds |> ignore;
                solve seeds mapped input         
            end
            | _ -> failwith "parsing error"
        end
        | exception End_of_file -> mapped


let start_seeds = input_line input |> stripl_or_panic "seeds: " |> String.split_on_char ' ' |> List.map int_of_string |> Array.of_list
let () = Batteries.dump start_seeds |> print_endline
let () = print_int @@ Array.fold_right1 min @@ solve start_seeds (Array.copy start_seeds) input
