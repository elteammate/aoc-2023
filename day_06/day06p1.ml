[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-33"]
[@@@warning "-34"]
[@@@warning "-37"]
[@@@warning "-69"]

open Shared 
open Extensions

let input = Input.require_input_file ()

let times = input_line input |> stripl_or_panic "Time:" |> String.split_on_char' ' ' |> List.map int_of_string |> Array.of_list
let distances = input_line input |> stripl_or_panic "Distance:" |> String.split_on_char' ' ' |> List.map int_of_string |> Array.of_list

let races = Array.combine times distances

let solve (time, distance) = 
    let rec aux = 
        function
        | 0 -> 0
        | t -> aux (t - 1) + (int_of_bool ((time - t) * t > distance))
    in aux time

let results = Array.map solve races
let () = Batteries.dump results |> print_endline
let () = Array.fold_left ( * ) 1 results |> print_int
