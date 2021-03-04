// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open NUnit.Framework
open FsUnit
open Parser

[<Test>]
let ``test hello``() = 
    5 + 1 |> should equal 6

[<EntryPoint>]
let main argv = 
    let res1 = readExpr "\"this is a string\""
    let res2 = readExpr "25"
    let res3 = readExpr "symbol"
    let res4 = readExpr "(symbol)"
    printfn "res1\t%s\n" res1
    printfn "res2\t%s\n" res2
    printfn "res3\t%s\n" res3
    printfn "res4\t%s\n" res4

    let res5 = readExpr "(a test)"
    let res6 = readExpr "(a (nested) test)"
    let res7 = readExpr "(a (dotted . list) test)"
    let res8 = readExpr "(a '(quoted (dotted . list)) test)"
    let res9 = readExpr "(a '(imbalanced parens)"
    printfn "res5\t%s\n" res5
    printfn "res6\t%s\n" res6
    printfn "res7\t%s\n" res7
    printfn "res8\t%s\n" res8
    printfn "res9\t%s\n" res9

    0 // return an integer exit code
