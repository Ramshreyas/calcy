module scratch

open System

type Result<'a, 'b> = 
    | Success of 'a
    | Failure of 'b

type Parser<'a> = Parser of (char list -> Result<'a * char list, string>)

let run parser input =
    let (Parser parserFn) = parser
    parserFn input

let charParser expectedChar = 
    let innerFn inputChars =
        match inputChars with
        | c::remainingChars ->
            if c = expectedChar then Success (c, remainingChars)
            else Failure (sprintf "expected %c, got %c" expectedChar c)
        | [] ->
            Failure (sprintf "No input")
    Parser innerFn

let stringToCharList str =
    List.ofSeq str

stringToCharList "lake"
|> run (charParser 'l')
|> printfn "%A"

let env = Map.empty

let evaluate env input =
    env, input

let respond env input =
    let newEnv, response = evaluate env input
    printfn "> %s" response
    newEnv

let rec repl env =
    let newInput = Console.ReadLine()
    let newEnv = respond env newInput
    repl newEnv

repl env

