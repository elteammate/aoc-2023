[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-34"]
[@@@warning "-37"]
[@@@warning "-69"]

let input_filename = match Sys.argv with 
    | [| _; f |] -> f
    | _ -> failwith "Expected exactly one argument: input filename"

let rec range n = if n < 0 then [] else n :: range (n - 1)

let swap a i j = let temp = a.(i) in begin
    a.(i) <- a.(j);
    a.(j) <- temp;
end

let shuffle a = let n = Array.length a in begin
    for i = 0 to n - 1 do
        let j = Random.int (n - i) in
        swap a i (n - j - 1);
    done;
end

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

module ANSI = struct
    let esc = "\x1B"
    let reset = esc ^ "[0m"
    let bold = esc ^ "[1m"
    let bold_reset = esc ^ "[22m"
    let italic = esc ^ "[3m"
    let italic_reset = esc ^ "[23m"
    let underline = esc ^ "[4m"
    let underline_reset = esc ^ "[24m"
    type t_color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default

    let fg c = match c with
        | Black -> esc ^ "[30m"
        | Red -> esc ^ "[31m"
        | Green -> esc ^ "[32m"
        | Yellow -> esc ^ "[33m"
        | Blue -> esc ^ "[34m"
        | Magenta -> esc ^ "[35m"
        | Cyan -> esc ^ "[36m"
        | White -> esc ^ "[37m"
        | Default -> esc ^ "[39m"

    let bg c = match c with
        | Black -> esc ^ "[40m"
        | Red -> esc ^ "[41m"
        | Green -> esc ^ "[42m"
        | Yellow -> esc ^ "[43m"
        | Blue -> esc ^ "[44m"
        | Magenta -> esc ^ "[45m"
        | Cyan -> esc ^ "[46m"
        | White -> esc ^ "[47m"
        | Default -> esc ^ "[49m"

    let clear_all = esc ^ "[2J" ^ esc ^ "[H"
    let move y x = esc ^ "[" ^ string_of_int y ^ ";" ^ string_of_int x ^ "H"
end 

let rec repeat s n = if n > 0 then s ^ repeat s (n - 1) else ""

type color = Red | Green | Blue

let machine_layers = 8
let cup_width = 9
let cup_height = 10
let hole_width = 3
let half_hole_width = hole_width / 2
let internal_height = machine_layers * 4 + 1
let machine_height = internal_height + cup_height + 1
let internal_width = cup_width * 3 + 2
let machine_width = internal_width + 2
let internal_half_width = internal_width / 2

type cube = {
    x: int;
    y: int;
    color: color;
}

type protocube = {
    x: int;
    y: int;
    id: int;
}

let rec simulation cubes = let result = Array.copy cubes in
    let free y x = 
        Array.for_all (fun {x = x2; y = y2; id = _} -> x != x2 || y != y2) result &&
        if y <= 0 then 
            x >= internal_half_width - half_hole_width && x <= internal_half_width + half_hole_width
        else if y < internal_height - 2 && y mod 4 = 1 then
            x >= 0 && x < internal_width && x mod 2 = 0
        else if y < internal_height - 2 && y mod 4 = 3 then
            x >= 0 && x < internal_width && x mod 2 = 1
        else if y < internal_height - 1 then
            x >= 0 && x < internal_width 
        else if y < internal_height + cup_height - 1 then
            x >= 0 && x < internal_width && x <> cup_width && x <> cup_width * 2 + 1
        else
            false
    in
    let changed, _ = Array.fold_left (fun (changed, i) {x = x; y = y; id = id} -> 
        let down = free (y + 1) x in
        let right = free (y + 1) (x + 1) in
        let left = free (y + 1) (x - 1) in
        let rnd = Random.int 10 in
        begin
            if rnd < 3 && down then
                result.(i) <- {x = x; y = y + 1; id = id}
            else if rnd < 6 && right then
                result.(i) <- {x = x + 1; y = y + 1; id = id}
            else if rnd < 9 && left then
                result.(i) <- {x = x - 1; y = y + 1; id = id}
            else
                ()
        end;
        changed || (left || right || down), i + 1
    ) (false, 0) cubes
    in if not changed then [result] else result :: simulation result

let simulation_continue frames = 
    let last_frame = List.hd @@ List.rev frames in
    let prev_frame = ref (List.hd frames) in
    List.map (fun frame -> 
        let res_frame = Array.map (fun ({x = x; y = y; id = id} as c) -> 
            if (Option.get (Array.find_opt (fun c -> c.id = id) last_frame)).y > y then
                c
            else
                let {x = x; y = y; id = id} as c = Option.get (Array.find_opt (fun c -> c.id = id) !prev_frame) in
                if y < internal_height + cup_height - 2 && Array.for_all (fun ({x = cx; y = cy; id = _}) -> x <> cx || y + 1 <> cy) !prev_frame then
                    {x = x; y = y + 1; id = id}
                else
                    c
        ) frame in
            prev_frame := res_frame;
            res_frame
    ) frames

let cherrypicked_from_simulation r g b sim =
    let last_frame = List.hd @@ List.rev sim in
    let (lred, lgreen, lblue) = Array.fold_left (fun (red, green, blue) ({x = x; y = _; id = _} as c) ->
        if x <= cup_width then
            (c :: red, green, blue)
        else if x <= cup_width * 2 then
            (red, c :: green, blue)
        else
            (red, green, c :: blue)
    ) ([], [], []) last_frame in
    let (red, green, blue) = (Array.of_list lred, Array.of_list lgreen, Array.of_list lblue) in begin
        shuffle red;
        shuffle green;
        shuffle blue;
        if Array.length red < r || Array.length green < g || Array.length blue < b then
            None
        else
            let red_ids = Array.map (fun x -> x.id) @@ Array.sub red 0 r in
            let green_ids = Array.map (fun x -> x.id) @@ Array.sub green 0 r in
            let blue_ids = Array.map (fun x -> x.id) @@ Array.sub blue 0 r in
            let ids = Array.concat [red_ids; green_ids; blue_ids] in
            let result = (
                List.map 
                    (fun frame -> Array.of_list @@ List.filter (fun x -> Array.find_opt ((=) x.id) ids <> None) @@ Array.to_list frame) 
                    sim
            ) in 
            let rec aux res = match res with
            | [cubes] -> List.map (fun _ -> cubes) (range 5)
            | head :: ax -> head :: aux ax
            | [] -> failwith "Unexpected empty simulation" in
            let result = simulation_continue @@ aux result in
            Some (List.map (Array.map (fun {x = x; y = y; id = id} ->
                    let color = if Array.find_opt ((=) id) red_ids <> None then
                        Red
                    else if Array.find_opt ((=) id) blue_ids <> None then
                        Blue
                    else
                        Green
                    in {x = x; y = y; color = color}
                )) result)
    end
    

let draw_cube by bx {x = x; y = y; color = c} = 
    print_string @@ (ANSI.move (by + y) (bx + x)) 
        ^ (match c with
            | Red -> ANSI.fg ANSI.Red
            | Green -> ANSI.fg ANSI.Green
            | Blue -> ANSI.fg ANSI.Blue)
        ^ "■" ^ ANSI.reset

let draw_machine by bx cubes =
    let draw_walls y = 
        print_string @@ (ANSI.move (by + y) bx) ^ "┃"
            ^ (repeat " " internal_width) ^ "┃"
    in begin
        print_string ANSI.reset;
        print_string @@ (ANSI.move by bx) ^ "┏"
            ^ (repeat "━" (internal_half_width - half_hole_width)) ^ (repeat " " hole_width)
            ^ (repeat "━" (internal_half_width - half_hole_width)) ^ "┓";
        for i = 0 to machine_layers - 1 do
            draw_walls (i * 4 + 1);
            print_string @@ (ANSI.move (by + i * 4 + 2) bx) ^ "┃"
                ^ (repeat " ♦" internal_half_width) ^ " ┃";
            draw_walls (i * 4 + 3);
            print_string @@ (ANSI.move (by + i * 4 + 4) bx) ^ "┃"
                ^ (repeat "♦ " internal_half_width) ^ "♦┃";
        done;
        draw_walls (machine_layers * 4);
        for i = 0 to cup_height - 1 do 
            print_string @@ (ANSI.move (by + internal_height + i) bx) ^ "┃"
                ^ (repeat " " cup_width) ^ "┃"
                ^ (repeat " " cup_width) ^ "┃"
                ^ (repeat " " cup_width) ^ "┃";
        done;
        print_string @@ (ANSI.move (by + machine_height - 1) bx) ^ "┣" 
            ^ (repeat "━" cup_width) ^ "╋"
            ^ (repeat "━" cup_width) ^ "╋"
            ^ (repeat "━" cup_width) ^ "┫";
        print_string @@ (ANSI.move (by + machine_height) bx) ^ "┃"
            ^ (repeat " " cup_width) ^ "┃"
            ^ (repeat " " cup_width) ^ "┃"
            ^ (repeat " " cup_width) ^ "┠" ^ (repeat "─" 10);
        print_string @@ (ANSI.move (by + machine_height + 1) bx) ^ "┗" 
            ^ (repeat "━" cup_width) ^ "┻"
            ^ (repeat "━" cup_width) ^ "┻"
            ^ (repeat "━" cup_width) ^ "┛";
        Array.map (draw_cube (by + 1) (bx + 1)) cubes |> ignore;
        let r, g, b = Array.fold_left (fun (r, g, b) {x = x; y = y; color = _} -> 
            if x < cup_width then 
                (r + 1, g, b)
            else if x > cup_width && x < 2 * cup_width + 1 then
                (r, g + 1, b)
            else if x > cup_width * 2 + 1 then
                (r, g, b + 1)
            else 
                (r, g, b)
        ) (0, 0, 0) cubes in begin
            print_string @@ ANSI.move (by + machine_height) (bx + cup_width / 2 + 1) ^ ANSI.fg ANSI.Red ^ string_of_int r;
            print_string @@ ANSI.move (by + machine_height) (bx + 3 * cup_width / 2 + 2) ^ ANSI.fg ANSI.Green ^ string_of_int g;
            print_string @@ ANSI.move (by + machine_height) (bx + 5 * cup_width / 2 + 3) ^ ANSI.fg ANSI.Blue ^ string_of_int b;
            (r, g, b)
        end
    end

let simulate_and_draw by bx initial = 
    let sim = simulation initial
    in List.map (fun frame -> begin
        print_string ANSI.clear_all;
        draw_machine by bx @@
            Array.map (fun {x = x; y = y; id = _} -> {x = x; y = y; color = Red}) frame |> ignore;
        Thread.delay 0.1;
        flush stdout;
    end) sim |> ignore

let draw by bx sim =
    List.map (fun frame -> begin
        print_string ANSI.clear_all;
        draw_machine by bx frame |> ignore;
        Thread.delay 0.03;
        flush stdout;
    end) sim |> ignore

let rec draw_many by bx sims =
    print_string ANSI.clear_all;
    if Array.for_all (fun e -> List.length e = 1) sims then
        ()
    else begin
        print_string ANSI.clear_all;
        let next_sim = Array.mapi (fun i sim -> begin 
            draw_machine by (bx + i * (machine_width + 1)) (List.hd sim) |> ignore;
            match sim with
            | [] -> failwith "impossible"
            | [e] -> [e]
            | s :: rest -> rest
        end) sims in begin
            flush stdout;
            Thread.delay 0.03;
            draw_many by bx next_sim
        end
    end

let play_game game = 
    let (_game_id, description) = 
        match String.split_on_char ':' game with
        | [game_id_str; game_description] -> 
            int_of_string @@ stripl_or_panic "Game " game_id_str, 
            List.map (fun round_description -> 
                List.fold_left (fun (r, g, b) cubes -> 
                    let cubes = String.trim cubes 
                    in match (stripr " red" cubes, stripr " green" cubes, stripr " blue" cubes) with
                    | (Some c), _, _ -> (r + int_of_string c, g, b)
                    | _, (Some c), _ -> (r, g + int_of_string c, b)
                    | _, _, (Some c) -> (r, g, b + int_of_string c)
                    | _ -> (r, g, b)
                ) (0, 0, 0) (String.split_on_char ',' round_description)
            ) (String.split_on_char ';' game_description)
        | _ -> failwith "Unexpected input format" in 
    let description = Array.of_list description in
    let description = Array.append description (Array.make (6 - Array.length description) (0, 0, 0)) in
    begin
        shuffle description;
        draw_many 5 5 @@ Array.map (fun (r, g, b) -> 
            range 150
                |> List.map (fun id -> {x = internal_half_width; y = -5; id = id})
                |> Array.of_list
                |> simulation
                |> cherrypicked_from_simulation r g b
                |> Option.get
        ) description;
        0
    end

let rec play_all input = 
    match input_line input with
    | game -> max (play_game game) @@ play_all input
    | exception End_of_file -> 0

let () = input_filename |> open_in |> play_all |> print_int

