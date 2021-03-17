module LispTypes

let unwordsList ls = ls |> List.map (fun l -> l.ToString()) |> String.concat " "

type LispVal = 
    | LispAtom of string
    | LispList of List<LispVal>
    | LispDottedList of List<LispVal> * LispVal
    | LispNumber of int64
    | LispString of string
    | LispBool of bool

    override this.ToString() = 
        match this with
        | LispAtom s -> s
        | LispString s -> sprintf "\"%s\"" s
        | LispNumber v -> sprintf "%d" v
        | LispBool true -> "#t"
        | LispBool false -> "#f"
        | LispList v -> unwordsList v |> sprintf "(%s)"
        | LispDottedList (h, t) -> h |> unwordsList |> sprintf "( %s . %s)" <| (t.ToString())