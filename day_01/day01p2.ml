let digit_of_char = function
    | ('1'..'9' as c) -> Some (int_of_char c - int_of_char '0')
    | _ -> None

(* let first_and_last_digits = String.fold_left 
    (fun a c -> match (a, digit_of_char c) with
        | (a, None) -> a
        | (None, Some d) -> Some (d, d)
        | (Some (first, _), Some d) -> Some (first, d)
    ) None *)

let digits = [| 
    "zero";
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine";
|]

let first_and_last_digits_plus s = 
let extract_digit i = if '0' <= s.[i] && s.[i] <= '9' then 
    digit_of_char s.[i] 
else 
    Array.find_index (fun digit -> try String.sub s i (String.length digit) = digit with Invalid_argument _ -> false) digits
in let rec 
    helper i = if i >= String.length s then 
        None 
    else 
        match helper (i + 1), extract_digit i with
        | prev, None -> prev
        | None, Some d -> Some (d, d)
        | Some (_, last), Some d -> Some (d, last)
in helper 0

let rec solve input = let
    line = try Some (input_line input) with End_of_file ->
        close_in input;
        None
    in match line with
    | None -> 0
    | Some line -> let (first, last) = match first_and_last_digits_plus line with
        | Some x -> x
        | None -> failwith "Expected the string to contain some digit"
        in first * 10 + last + solve input


let input_filename = match Sys.argv with 
    | [| _; f |] -> f
    | _ -> failwith "Expected exactly one argument: input filename"

let () = 
    let input_file = open_in input_filename in
    Printf.printf "%d\n" @@ solve input_file

