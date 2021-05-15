// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open NUnit.Framework
open FsUnit
open Parser
open Eval
[<Test>]
let ``test hello``() = 
    5 + 1 |> should equal 6

let interpret (expr: string) : Unit =
    printfn "Evaluating expression: %s" expr
    let ast = readExpr expr // |> Array.tryHead |> Option.defaultValue "" |> readExpr 
    match ast with 
    | Result.Ok a -> printfn "%A" a
    | Result.Error err -> printfn "%A" err
    let result = ast |> Result.bind eval
    match result with 
    | Result.Ok v -> printfn "evaluated: %s" (v.ToString())
    | Result.Error e -> printfn "error: %s" (e.ToString())

[<EntryPoint>]
let main argv = 

    let inp = [|"(string<? \"abc\" \"bba\")"; 
                "(if (> 2 3) \"no\" \"yes\")"; 
                "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")";
                "(cdr '(a simple test))";
                "(car (cdr '(a simple test)))";
                "(cons '(this is) 'test)"
                |]
    Array.iter interpret inp
    0 // return an integer exit code
