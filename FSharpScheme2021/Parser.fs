module Parser

open LispTypes
open FParsec
open NUnit.Framework
open FsUnit

type LispState = unit
type Parser<'T> = Parser<'T, LispState>

let pSymbol: Parser<_> = anyOf "!#$%&|*+-/:<=>?@^_~"

let notQuoteChar = noneOf (Seq.toList "\"")
let unquotedString = manyChars notQuoteChar
let betweenQuotes = between (pstring "\"") (pstring "\"")
let parseString: Parser<LispVal> = betweenQuotes unquotedString |>> LispString

let parseAtom = 
    pipe2 (letter <|> pSymbol)
          (manyChars (letter <|> digit <|> pSymbol))
          (fun s rest ->
                let atom = sprintf "%c%s" s rest
                match atom with
                | "#t" -> LispBool true
                | "#f" -> LispBool false
                | _ -> LispAtom atom)

let parseNumber: Parser<_> = pint64 |>> LispNumber

let parseExpr, parseExprRef = createParserForwardedToRef()

let parseList = sepBy parseExpr spaces1 |>> LispList

let parseDottedList = pipe2 (sepEndBy parseExpr spaces1) 
                            (pchar '.' >>. spaces >>. parseExpr) 
                            (fun head tail -> LispDottedList(head, tail))

let parsedQuoted = pchar '\'' >>. parseExpr |>> (fun expr -> [ LispAtom "quote"; expr] |> LispList)

parseExprRef := choice [parseAtom
                        parseString
                        parseNumber
                        parsedQuoted
                        (between (pchar '(') (pchar ')') (attempt parseList <|> parseDottedList))]

let readExpr input = 
    match run (spaces >>. parseExpr .>> eof) input with 
    | Failure (err, _, _) -> sprintf "No match: %s" err |> ParseError |> throwError
    | Success (e, _, _) -> Result.Ok e //e //sprintf "Found Value: %s" (e.ToString())

let checkResult v r = match r with
                      | ParserResult.Success(e, _, _) -> e |> should equal v
                      | _ -> Assert.Fail "parse failed"

let checkParseFailed r = match r with
                         | ParserResult.Success(_, _, _) -> Assert.Fail "expected parse to fail"
                         | _ -> ()
                     
[<Test>]
let ``parse atom test`` () =
    run parseAtom "#t" |> checkResult (LispBool true)
    run parseAtom "#f" |> checkResult (LispBool false)
    run parseAtom "#test" |> checkResult (LispAtom "#test")
    run parseAtom "test" |> checkResult (LispAtom "test")
    run parseAtom "+" |> checkResult (LispAtom "+")
    run parseAtom "1" |> checkParseFailed