module In = struct
    external init  : unit -> bool = "caml_init_shell"
    external exit  : unit -> bool = "caml_exit_shell"
    external readc : unit -> int  = "caml_readc"

    let readc () : int option =
        match readc () with
        | -1 | -2 -> None
        | -3      -> assert false
        | n       -> Some n
end

module Ansi = struct
    type d = Left | Up | Right | Down

    type a =
        | Unknown of string
        | Arrow   of d
        | Char    of char

    type e = Node of (int * e) list | Leaf of a

    let escape =
        Node [
            0x5B, (* [ *)
            Node [
                0x41, Leaf (Arrow Up);          (* [A *)
                0x42, Leaf (Arrow Down);        (* [B *)
                0x43, Leaf (Arrow Right);       (* [C *)
                0x44, Leaf (Arrow Left);        (* [D *)
            ];
        ]

    let pp_ansi ppf (a : a) =
        let pp = Format.fprintf in
        match a with
        | Unknown s -> pp ppf "unknown(%s)" s
        | Arrow _   -> pp ppf "a"
        | Char c    -> pp ppf "%c" c


    let read_escape () : a =
        let b = Buffer.create 2 in
        let (let*) o f =
            match o with
            | None   -> Unknown (Buffer.contents b)
            | Some i -> f i
        in
        let rec aux (e : e) (b : Buffer.t) =
            match e with
            | Leaf i -> i
            | Node l ->
                let* i = In.readc () in
                let* (_, e) = List.find_opt (fun (j, _) -> i = j) l in
                aux e b
        in
        aux escape b

    let input () : a option =
        match In.readc () with
        | None      -> None
        | Some 0x1B -> Some (read_escape ())
        | Some c    -> Some (Char (Char.chr c))
end

let init  = In.init
let exit  = In.exit
let input = Ansi.input

