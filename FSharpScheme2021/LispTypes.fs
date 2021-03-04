module LispTypes

type LispVal = 
    | LispAtom of string
    | LispList of List<LispVal>
    | LispDottedList of List<LispVal> * LispVal
    | LispNumber of int64
    | LispString of string
    | LispBool of bool
