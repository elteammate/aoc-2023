[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-33"]
[@@@warning "-34"]
[@@@warning "-37"]
[@@@warning "-69"]

open Shared 
open Extensions

let input = Input.require_input_file ()

let time = input_line input |> stripl_or_panic "Time:" |> String.split_on_char' ' ' |> String.concat "" |> int_of_string
let distance = input_line input |> stripl_or_panic "Distance:" |> String.split_on_char' ' ' |> String.concat "" |> int_of_string

let optimal = time / 2

let rec find_low l r = 
    if r = l + 1 then
        r
    else let m = (l + r) / 2 in
        if m * (time - m) > distance then
            find_low l m
        else
            find_low m r

let low = find_low 0 optimal
let high = time - low

let result = high - low + 1
let () = result |> print_int
