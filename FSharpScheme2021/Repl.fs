module Repl

open System
open Eval
open Parser

let readPrompt (prompt: string) = 
    Console.Write prompt
    let v = Console.ReadLine()
    if isNull v then "" else v

let readAndEval env = readExpr >> Result.bind (eval env)

let evalString env expr = 
    match readAndEval env expr with
    | Ok v -> v.ToString()
    | Error e -> sprintf "Eval failed: %s" (e.ToString())

let evalAndPrint env expr = evalString env expr |> Console.WriteLine

let rec until pred prompt action = 
    let input = prompt()
    if not (pred input) then
        action input
        until pred prompt action

let runOne expr = evalAndPrint (nullEnv()) expr

let runRepl () =
    until ((=) "quit") (fun () -> readPrompt "Lisp>>> ") (evalAndPrint (nullEnv()))

