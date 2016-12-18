﻿open System

//--------------PARSER TYPES----------------

type Result<'a, 'b> = 
    | Success of 'a
    | Failure of 'b

type Parser<'a> = Parser of (char list -> Result<'a * char list, string>)

type Atom =
    | Variable of string
    | Value of decimal

type Operator =
    | Addition of string
    | Subtraction of string
    | Multiplication of string
    | Division of string
    | Equals of string

type Expression<'a> =
    | Atom
    | E of Expression<'a> * Operator * Expression<'a>
 
//--------------PARSER COMBINATORS----------------

let run parser inputChars =
    let (Parser parserFn) = parser
    parserFn inputChars

let charParser expectedChar = 
    let innerFn inputChars =
        match inputChars with
        | c::remainingChars ->
            if c = expectedChar then Success (c, remainingChars)
            else Failure (sprintf "expected %c, got %c" expectedChar c)
        | [] ->
            Failure (sprintf "No input")
    Parser innerFn

let orParse parser1 parser2 =
    let innerFn inputChars =
        match run parser1 inputChars with
        | Success result -> Success result
        | Failure _ -> run parser2 inputChars
    Parser innerFn

let ( <|> ) = orParse

let choice parserList =
    List.reduce orParse parserList

let anyOfParser charList = 
    charList
    |> List.map charParser
    |> choice

let andParse parser1 parser2 =
    let innerParser charList =
        match run parser1 charList with
        | Failure result -> Failure result
        | Success (c1, remainingCharList1) ->
            match run parser2 remainingCharList1 with
            | Failure result -> Failure result
            | Success (c2, remainingCharList2) ->
                Success ((c1, c2), remainingCharList2)
    Parser innerParser

let ( .>>. ) = andParse

let pMap mapFn parser =
    let innerFn charList = 
        match run parser charList with
        | Failure result -> Failure result
        | Success (c, remainingChars) ->
            Success ((mapFn c), remainingChars)
    Parser innerFn

let ( <!> ) = pMap

let ( |>> ) x f = pMap f x

let pReturn value =
    let innerFn inputChars =
        Success (value, inputChars)
    Parser innerFn

let pApply parserFn parserVal = 
    (parserFn .>>. parserVal)
    |> pMap (fun (f, x) -> f x)

let ( <*> ) = pApply

let ( .>> ) p1 p2 =
    p1 .>>. p2
    |> pMap (fun (a, b) -> a)

let ( >>. ) p1 p2 =
    p1 .>>. p2
    |> pMap (fun (a, b) -> b)

let lift2 twoParameterFn parser1 parser2 = 
    pReturn twoParameterFn <*> parser1 <*> parser2

let rec batchParser parsers = 
    let concat head tail = head :: tail
    let pConcat = lift2 concat

    match parsers with
    | [] -> pReturn []
    | parser :: remainingParsers ->
        pConcat parser (batchParser remainingParsers)

let rec parseZeroOrMore parser chars =
    match run parser chars with
    | Failure msg -> ([], chars)
    | Success (firstValue, remainingChars1) ->
        let (subsequentValues, remainingChars2) =
            parseZeroOrMore parser remainingChars1
        let values = firstValue::subsequentValues
        (values, remainingChars2)

let many parser =
    let rec innerParser chars =
        Success (parseZeroOrMore parser chars)
    Parser innerParser

let many1 parser =
    let rec innerParser chars =
        match run parser chars with
        | Failure msg -> Failure msg
        | Success (firstValue, remainingChars1) ->
            let (subsequentValues, remainingChars2) =
                parseZeroOrMore parser remainingChars1
            let values = firstValue::subsequentValues
            Success (values, remainingChars2)
    Parser innerParser

let opt p =
    let some = p |>> Some
    let none = pReturn None
    some <|> none

//------------------------HELPER FUNCTIONS-----------------

let stringToCharList str =
    List.ofSeq str

let charListToString chars =
    System.String(List.toArray chars)

let charListToDecimal chars =
    let decimalString = charListToString chars
    decimal decimalString

let signedCharListToDecimal (sign, chars) =
    let decimal = charListToDecimal chars
    match sign with
    | Some _ -> -decimal
    | None -> decimal

//-------------------------Parsers------------------------------

let whiteCharParser = anyOfParser [' '; '\t']

let whitespaceParser = many whiteCharParser

let digitParser = anyOfParser ['0'..'9']

let digitsParser = many1 digitParser

let decimalParser = opt (charParser '-') .>>. digitsParser |>> signedCharListToDecimal

let alphabetParser = anyOfParser ['a'..'z']

let stringParser = many1 alphabetParser |> pMap charListToString

let operatorParser = 
    let innerParser chars =
        match run (anyOfParser ['+'; '-'; '*'; '/'; '=']) chars with
        | Failure msg -> Failure msg
        | Success (operator, remainingChars) ->
            match operator with
                | '+' -> Success (Addition("+"), remainingChars)
                | '-' -> Success (Subtraction("-"), remainingChars)
                | '*' -> Success (Multiplication("*"), remainingChars)
                | '/' -> Success (Division("/"), remainingChars)
                | '=' -> Success (Equals("="), remainingChars)
    Parser innerParser
        
let variableParser = stringParser

let valueParser = decimalParser

let result1 = run operatorParser (stringToCharList "=1 ab ")

//----------------------INTERPRETER----------------------------

let env = Map.empty

let evaluate env input =
    env, input

let respond env input =
    let newEnv, response = evaluate env input
    printfn ":> %s" response
    newEnv

let rec repl env =
    printf "<: "
    let newInput = Console.ReadLine()
    let newEnv = respond env newInput
    repl newEnv

repl env

