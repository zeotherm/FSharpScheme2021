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

    let inp = [|"(+ 2 2)"|]
    inp 
    |> Array.tryHead 
    |> Option.defaultValue "" 
    |> readExpr 
    |> eval 
    |> (fun v -> v.ToString()) 
    |> printfn "%s\n"
    0 // return an integer exit code
