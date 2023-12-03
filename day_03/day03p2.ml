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

let is_gear c = c = '*'

let result = begin
    let touched = Array.map (fun s -> s |> String.to_array |> Array.map (fun _ -> [])) input in
    Array.mapi (fun i line -> 
        let _, _ = String.fold_lefti (fun j (cur_num, len) c -> 
            let print_non_digit c = begin
                    if is_gear c then
                        print_string @@ Ansi.fg Ansi.Red ^ String.of_char c ^ Ansi.fg Ansi.Default
                    else if is_special c then
                        print_string @@ Ansi.fg Ansi.Blue ^ String.of_char c ^ Ansi.fg Ansi.Default
                    else
                        print_string @@ Ansi.fg Ansi.Black ^ String.of_char c ^ Ansi.fg Ansi.Default
            end in
            match cur_num with
            | None when is_digit c -> Some (to_digit c), 1
            | Some num when is_digit c -> Some (10 * num + to_digit c), len + 1
            | Some num -> begin
                begin
                    let check_gear_and_count i j =
                        if i < 0 || i >= Array.length input then
                            false
                        else if j < 0 || j >= String.length input.(i) then
                            false
                        else if is_gear input.(i).[j] then begin
                            touched.(i).(j) <- num :: touched.(i).(j);
                            true
                        end else
                            false
                    in begin
                        check_gear_and_count i j |> ignore;
                        check_gear_and_count i (j - len - 1) |> ignore;
                        List.map (fun k -> check_gear_and_count (i - 1) (j - k)) (0 -- (len + 2)) |> ignore;
                        List.map (fun k -> check_gear_and_count (i + 1) (j - k)) (0 -- (len + 2)) |> ignore;
                        print_string @@ Ansi.fg Ansi.Green ^ string_of_int num ^ Ansi.fg Ansi.Default;
                        print_non_digit c;
                        None, 0
                    end
                end
            end
            | None -> begin
                print_non_digit c;
                None, 0
            end
        ) (None, 0) (line ^ "\n") in ()
    ) input |> ignore;
    Array.fold_left (fun sum line -> 
        Array.fold_left (fun sum numbers -> 
            if List.length numbers <> 2 then
                sum
            else
                sum + List.fold_left ( * ) 1 numbers
        ) sum line
    ) 0 touched
end

let () = print_int result

