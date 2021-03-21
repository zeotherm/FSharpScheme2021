module LispTypes

let unwordsList ls = ls |> List.map (fun l -> l.ToString()) |> String.concat " "

type LispVal = 
    | LispAtom of string
    | LispList of LispVal list
    | LispDottedList of LispVal list * LispVal
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

type LispError = 
    | NumArgs of int32 * LispVal list
    | TypeMismatch of string * LispVal
    | ParseError of string
    | BadSpecialForm of string * LispVal
    | NotFunction of string * string
    | UnboundVar of string * string
    | UnspecifiedReturn of string
    | DefaultError of string

    override this.ToString() = 
        match this with
        | NumArgs (expected, found) ->
            sprintf 
                "Expected %d args; found values: %s"
                expected
                (found |> List.map (fun v -> v.ToString()) |> String.concat " ")
        | TypeMismatch (expected, found) -> sprintf "Invalid type. Expected %s, found %s" expected (found.ToString())
        | ParseError err -> sprintf "Parse error at %s" (err.ToString())
        | BadSpecialForm (message, form) -> sprintf "%s: %s" message (form.ToString())
        | NotFunction (message, func) -> sprintf "%s: %s" message func
        | UnboundVar (message, varname) -> sprintf "%s: %s" message varname
        | UnspecifiedReturn message -> message
        | DefaultError e -> sprintf "Error: %s" e

   
type ThrowsError<'T> = Result<'T, LispError>
let throwError e = Result.Error e
