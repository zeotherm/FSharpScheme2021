module LispTypes

let unwordsList ls = ls |> List.map (fun l -> l.ToString()) |> String.concat " "

[<CustomEqualityAttribute; NoComparisonAttribute>]
type LispVal = 
    | LispAtom of string
    | LispList of LispVal list
    | LispDottedList of LispVal list * LispVal
    | LispNumber of int64
    | LispString of string
    | LispBool of bool
    | LispPrimitiveFunc of (LispVal list -> ThrowsError<LispVal>)
    | LispFunc of string list * string option * LispVal list * Env

    override this.ToString() = 
        match this with
        | LispAtom s -> s
        | LispString s -> sprintf "\"%s\"" s
        | LispNumber v -> sprintf "%d" v
        | LispBool true -> "#t"
        | LispBool false -> "#f"
        | LispList v -> unwordsList v |> sprintf "(%s)"
        | LispDottedList (h, t) -> h |> unwordsList |> sprintf "( %s . %s)" <| (t.ToString())
        | LispPrimitiveFunc _ -> "<primitive>"
        | LispFunc (p, vargs, _, _) ->
            let paramStr = 
                p
                |> List.map (fun v -> v.ToString())
                |> String.concat " "
            let vargStr = 
                match vargs with
                | None -> ""
                | Some arg -> sprintf " . %s" arg
            sprintf """(lambda (%s%s) ... """ paramStr vargStr


    override x.Equals(yObj) = 
        let compareList (xs: LispVal list) (ys: LispVal list) = 
            xs.Length = ys.Length &&
            (List.zip xs ys |> List.forall (fun (x, y) -> x = y))
        
        match yObj with
        | :? LispVal as y ->
            match (x, y) with
            | (LispAtom s1, LispAtom s2) -> s1 = s2
            | (LispString s1, LispString s2) -> s1 = s2
            | (LispNumber n1, LispNumber n2) -> n1 = n2
            | (LispBool b1, LispBool b2) -> b1 = b2
            | (LispList s1, LispList s2) -> compareList s1 s2
            | (LispDottedList (h1, t1), LispDottedList(h2, t2)) -> compareList h1 h2 && t1 = t2
            | (LispPrimitiveFunc f1, LispPrimitiveFunc f2) -> LanguagePrimitives.PhysicalEquality f1 f2
            | (LispFunc (params1, varg1, body1, closure1), LispFunc (params2, varg2, body2, closure2)) ->
                params1 = params2
                && varg1 = varg2
                && body1 = body2
                && closure1 = closure2
            | _ -> false
and LispError = 
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
and Env = System.Collections.Generic.Dictionary<string, LispVal ref>
and ThrowsError<'T> = Result<'T, LispError>
// I don't like the use of the mutable variables here, that is a serious code smell

let throwError e = Result.Error e