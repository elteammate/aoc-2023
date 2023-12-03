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

