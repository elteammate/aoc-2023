[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-33"]
[@@@warning "-34"]
[@@@warning "-37"]
[@@@warning "-69"]

open Shared 
open Extensions

let input = 
    let file = Input.require_input_file () in
    let rec parse file =
        match input_line file with
        | line -> line :: parse file
        | exception End_of_file -> []
    in parse file |> Array.of_list

let is_digit c =
    match c with
    | '0'..'9' -> true
    | _ -> false

let to_digit c =
    match c with
    | '0'..'9' -> int_of_char c - int_of_char '0'
    | _ -> failwith @@ "Expected digit, found " ^ String.make 1 c

let is_special c =
    match c with
    | '0'..'9' | '.' | '\n' -> false
    | _ -> true 

let result = begin
    print_string Ansi.clear_all;
    Array.mapi (fun i line -> 
        let _, sum, _ = String.fold_lefti (fun j (cur_num, sum, len) c -> 
            let print_non_digit c = begin
                    if is_special c then
                        print_string @@ Ansi.fg Ansi.Red ^ String.of_char c ^ Ansi.fg Ansi.Default
                    else
                        print_string @@ Ansi.fg Ansi.Black ^ String.of_char c ^ Ansi.fg Ansi.Default
            end in
            match cur_num with
            | None when is_digit c -> Some (to_digit c), sum, 1
            | Some num when is_digit c -> Some (10 * num + to_digit c), sum, len + 1
            | Some num -> begin
                let ok = is_special c || 
                    (match line.[j - len - 1] with 
                    | c -> is_special c 
                    | exception Invalid_argument _ -> false)
                    || (match input.(i - 1) with
                    | line -> String.exists is_special @@ String.sub' line (j - len - 1) (len + 2)
                    | exception Invalid_argument _ -> false)
                    || (match input.(i + 1) with
                    | line -> String.exists is_special @@ String.sub' line (j - len - 1) (len + 2)
                    | exception Invalid_argument _ -> false)
                in begin
                    if ok then
                        print_string @@ Ansi.fg Ansi.Green ^ string_of_int num ^ Ansi.fg Ansi.Default
                    else
                        print_string @@ Ansi.fg Ansi.Yellow ^ string_of_int num ^ Ansi.fg Ansi.Default;
                    print_non_digit c;
                    None, sum + (if ok then num else 0), 0
                end
            end
            | None -> begin
                print_non_digit c;
                None, sum, 0
            end
        ) (None, 0, 0) (line ^ "\n") in sum
    ) input |> Array.fold_left (+) 0
end

let () = print_int result

