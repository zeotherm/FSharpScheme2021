module Eval

open LispTypes

let eval = 
    function
    | LispString _ as v -> v
    | LispNumber _ as v -> v
    | LispBool _ as v -> v
    | LispList [LispAtom "quote"; v ] -> v