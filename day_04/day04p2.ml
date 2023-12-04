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

let rec list_add l1 l2 =
    match l1, l2 with
    | a, [] -> a
    | [], a -> a
    | x::xs, y::ys -> (x + y) :: list_add xs ys

let rec solve duplicates input =
    match input_line input with
    | line -> begin
        let card_number, (winning, ours) = match String.split_on_char ':' line with
        | [card_descr; numbers_descr] -> begin
                int_of_string @@ String.trim @@ stripl_or_panic "Card" card_descr, 
                    match String.split_on_char '|' numbers_descr with
                        | [winning_descr; ours_descr] -> (
                                Array.map int_of_string @@ Array.of_list @@ String.split_on_char' ' ' winning_descr,
                                Array.map int_of_string @@ Array.of_list @@ String.split_on_char' ' ' ours_descr
                            )
                        | _ -> failwith @@ "Format error: " ^ numbers_descr
        end
        | _ -> failwith @@ "Format error: " ^ line
        in 
        let count, rest = match duplicates with 
            | [] -> 1, []
            | x :: rest -> 1 + x, rest
        in begin
            print_string @@ Ansi.fg Ansi.Blue ^ Printf.sprintf "%8d x " count
                ^ "Card " ^ Ansi.bold 
                ^ Printf.sprintf "%4d" card_number ^ Ansi.reset ^ ":" ^ Ansi.fg Ansi.Yellow
                ^ Array.fold_left (fun res num -> 
                    res ^ Printf.sprintf " %2d" num
                ) "" winning
                ^ Ansi.reset ^ " |";
            let score = Array.fold_left (fun score number -> 
                let wins = Array.exists ((=) number) winning in
                if wins then begin
                    print_string @@ Ansi.fg Ansi.Green ^ Printf.sprintf " %2d" number;
                    score + 1
                end else begin
                    print_string @@ Ansi.fg Ansi.Red ^ Printf.sprintf " %2d" number;
                    score
                end
            ) 0 ours 
            in begin
                print_string @@ Ansi.reset ^ " | " ^ Ansi.bold ^ Ansi.fg Ansi.Green ^ string_of_int score ^ Ansi.reset ^ "\n";
                count + solve 
                    (list_add (List.init score (fun _ -> count)) rest)
                    input
            end
        end
    end
    | exception End_of_file -> 0

let () = print_int @@ solve [] input
let () = print_newline ()
