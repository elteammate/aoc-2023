[@@@warning "-32"]

let input_filename = match Sys.argv with 
    | [| _; f |] -> f
    | _ -> failwith "Expected exactly one argument: input filename"

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

let play_game game = 
    let (game_id, description) = 
        match String.split_on_char ':' game with
        | [game_id_str; game_description] -> 
            int_of_string @@ stripl_or_panic "Game " game_id_str, 
            List.map (fun round_description -> 
                List.fold_left (fun (r, g, b) balls -> 
                    let balls = String.trim balls 
                    in match (stripr " red" balls, stripr " green" balls, stripr " blue" balls) with
                    | (Some c), _, _ -> (r + int_of_string c, g, b)
                    | _, (Some c), _ -> (r, g + int_of_string c, b)
                    | _, _, (Some c) -> (r, g, b + int_of_string c)
                    | _ -> (r, g, b)
                ) (0, 0, 0) (String.split_on_char ',' round_description)
            ) (String.split_on_char ';' game_description)
        | _ -> failwith "Unexpected input format"
    in
        if List.for_all (fun (r, g, b) -> r <= 12 && g <= 13 && b <= 14) description then
            game_id
        else
            0

let rec play_all input = 
    match input_line input with
    | game -> play_game game + play_all input
    | exception End_of_file -> 0

let () = input_filename |> open_in |> play_all |> print_int

