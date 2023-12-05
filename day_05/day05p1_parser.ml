[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-33"]
[@@@warning "-34"]
[@@@warning "-37"]
[@@@warning "-69"]

open Shared 
open Extensions

let input = Input.require_input_file ()

type parsing_error = 
    | Error of (int * string) lazy_t
    | Compound of (int * string * parsing_error list) lazy_t

type 'a parse_result =
    | Ok of ('a * int) list
    | Err of parsing_error

let raise_err e = Err (Error e)
let raise_comp e = Err (Compound e)
let ok p = Ok p

let char s i = 
    if i < String.length s then 
        Ok [s.[i], i + 1]
    else
        raise_err (lazy (i, "Unexpected end of string"))

let n_chars n s i =
    if i + n < String.length s then 
        Ok [String.sub s i n, i + n]
    else
        raise_err (lazy (i, "Unexpected end of string"))

let map f p s i = 
    match p s i with
    | Ok results -> ok @@ List.map (fun (res, i) -> f res, i) results
    | Err e -> raise_comp (lazy (i, "Failed to map results", [e]))

let filter_map f p s i =
    match p s i with
    | Ok results -> 
        begin
            match List.filter_map (fun (res, i) -> Option.map (fun x -> x, i) (f res)) results with
            | [] -> raise_err (lazy (i, "No patterns match the predicate"))
            | options -> ok options
        end
    | Err e -> raise_comp (lazy (i, "Failed to filter results", [e]))


let filter f p s i =
    match p s i with
    | Ok results -> 
        begin
            match List.filter (fun (res, i) -> f res) results with
            | [] -> raise_err (lazy (i, "No patterns match the predicate"))
            | options -> ok options
        end
    | Err e -> raise_comp (lazy (i, "Failed to filter results", [e]))

let or_error err p s i = match p s i with
    | Ok results -> ok results
    | Err e -> raise_comp (lazy (i, Lazy.force err, [e]))

let or_error' err p s i = match p s i with
    | Ok results -> ok results
    | Err e -> raise_comp (lazy (i, err, [e]))

let concat f p1 p2 s i = match p1 s i with
    | Err e -> raise_comp (lazy (i, "First parser in concatenation failed", [e]))
    | Ok results -> 
        let results = List.map (fun (res, j) -> 
            res, p2 s j
        ) results in
        let successes = List.flat_map (
            function
            | res1, Ok results -> List.map (fun (res2, j) -> f res1 res2, j) results
            | _, Err _ -> []
        ) results in match successes with
            | [] -> raise_comp (lazy (
                    i, 
                    "None of the attempts at parsing the second part of concatenation succeded:",
                    List.filter_map (function 
                        | _, Err e -> Some e
                        | _, Ok _ -> None
                    ) results
                ))
            | results -> Ok results

let concat_pair p1 p2 = concat (fun x y -> x, y) p1 p2 
let suffixed p1 p2 = concat (fun x y -> x) p1 p2
let prefixed p1 p2 = concat (fun x y -> y) p1 p2

let rec zero_or_more_greedy f a p s i = match p s i with
    | Ok results -> 
            ok @@ List.flat_map (fun (res, j) -> begin 
                if i = j then failwith "Parser in zero_or_more_greedy did not consume a single character";
                zero_or_more_greedy f a p s j |> (
                    function
                    | Ok res -> res
                    | Err _ -> failwith "Unreachable"
                ) |> List.map (fun (res_star, j) ->
                    f res res_star, j
                )
            end) results
    | Err e -> ok [a, i]

let zero_or_more_list p s i = zero_or_more_greedy (fun x a -> x :: a) [] p s i

let one_or_more_greedy f a p s i = 
    match zero_or_more_list p s i with
    | Err _ -> failwith "Unreachable"
    | Ok [[], _] -> raise_err (lazy (i, "Expected at least 1 match of the sub-parser, but got none"))
    | Ok lists -> ok @@ List.map (fun (l, i) -> List.fold_right f l a, i) lists 

let one_or_more_list p s i = one_or_more_greedy (fun x a -> x :: a) [] p s i

let one_or_more_foldr f p s i =
    match zero_or_more_list p s i with
    | Err _ -> failwith "Unreachable"
    | Ok [[], _] -> raise_err (lazy (i, "Expected at least 1 match of the sub-parser, but got none"))
    | Ok lists -> ok @@ List.map (fun (l, i) -> List.fold_right1 f l, i) lists 
 
let char_satisfy f = filter f char |> or_error' "Character does not match predicate"

let either p1 p2 s i = 
    match p1 s i, p2 s i with
    | Err e1, Err e2 -> raise_comp (lazy (i, "Both options in either have failed", [e1; e2]))
    | Ok res1, Err _ -> ok res1
    | Err _, Ok res2 -> ok res2
    | Ok res1, Ok res2 -> ok @@ List.append res1 res2

let either_prefer_first p1 p2 s i = 
    match p1 s i, p2 s i with
    | Err e1, Err e2 -> raise_comp (lazy (i, "Both options in either have failed", [e1; e2]))
    | Ok res1, _ -> ok res1
    | Err _, Ok res2 -> ok res2

let either_prefer_second p1 p2 s i = 
    match p1 s i, p2 s i with
    | Err e1, Err e2 -> raise_comp (lazy (i, "Both options in either have failed", [e1; e2]))
    | _, Ok res2 -> ok res2
    | Ok res1, Err _ -> ok res1

let ch c = char_satisfy ((=) c) |> or_error (lazy ("Expected " ^ String.of_char c))
let str s = filter ((=) s) (n_chars (String.length s)) |> or_error (lazy ("Expected " ^ s))
let sp = zero_or_more_greedy (fun c () -> ()) () @@ char_satisfy ((=) ' ')
let ws = zero_or_more_greedy (fun c () -> ()) () @@ char_satisfy (function | ' ' | '\t' | '\n' -> true | _ -> false)
let word = one_or_more_greedy (fun c s -> String.of_char c ^ s) "" @@ char_satisfy (function | 'a'..'z' | 'A'..'Z' -> true | _ -> false)

let digit = filter_map (
    function
        | '0'..'9' as d -> Some (int_of_char d - int_of_char '0')
        | _ -> None
) char |> or_error' "Character is not a digit"

let nat = one_or_more_foldr (fun d a -> a * 10 + d) digit

let (<*>) p1 p2 s i = (concat_pair p1 p2) s i
let ( *>) p1 p2 s i = (prefixed p1 p2) s i
let (<* ) p1 p2 s i = (suffixed p1 p2) s i
let (<+>) p1 p2 s i = either p1 p2 s i
let (<+) p1 p2 s i = either_prefer_first p1 p2 s i
let (+>) p1 p2 s i = either_prefer_second p1 p2 s i
let (#>) f p s i = map f p s i
let (%>) f p s i = map (fun (a, b) -> f a b) p s i
let (%%>) f p s i = map (fun ((a, b), c) -> f a b c) p s i
let (%%%>) f p s i = map (fun (((a, b), c), d) -> f a b c d) p s i

let run_parser p s = 
    match p s 0 with
    | Ok options -> List.filter (fun (res, i) -> i = String.length s) options 
    | Err err -> failwith "Parsing failed"

let rec expr () = sp *> p2_expr () <* sp
    and p2_expr () = (p1_expr ()) <+> (( + ) %> (p2_expr () <* sp <* ch '+' <*> p1_expr ())) <+> (( - ) %> (p2_expr () <* sp <* ch '-' <*> p1_expr ()))
    and p1_expr () = (p0_expr ()) <+> (( * ) %> (p1_expr () <* sp <* ch '*' <*> p0_expr ()))
    and p0_expr () = nat <+> ((fun x -> -x) #> (ch '-' *> sp *> p0_expr ())) <+> (ch '+' *> sp *> p0_expr ()) <+> (ch '(' *> sp *> p2_expr () <* sp <* ch ')')

let () = Batteries.dump (run_parser nat "12345678") |> print_endline
let () = Batteries.dump (run_parser (expr ()) "2 + 2 * 2") |> print_endline

