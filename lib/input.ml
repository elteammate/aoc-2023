let require_input_filename () = match Sys.argv with 
    | [| _; f |] -> f
    | _ -> failwith "Expected exactly one argument: input filename"

let require_input_file () = open_in @@ require_input_filename ()
