// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open NUnit.Framework
open FsUnit
open Parser
open Eval
[<Test>]
let ``test hello``() = 
    5 + 1 |> should equal 6

[<EntryPoint>]
let main argv = 

    let inp = [|"(saywhat? 2)"|]
    let result = inp |> Array.tryHead |> Option.defaultValue "" |> readExpr |> Result.bind eval
    
    match result with 
    | Result.Ok v -> printfn "evaluated: %s" (v.ToString())
    | Result.Error e -> printfn "error: %s" (e.ToString())
    0 // return an integer exit code
