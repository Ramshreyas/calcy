open System
open Microsoft.FSharp.Collections

//--------------TYPES----------------

type Result<'a, 'b> = 
    | Success of 'a
    | Failure of 'b

type Parser<'a> = Parser of (char list -> Result<'a * char list, string>)

type Operator =
    | Addition of string
    | Subtraction of string
    | Multiplication of string
    | Division of string
    | Equals of string

type Atom =
    | Variable of string
    | Value of decimal

type Expression =
    | Node of Atom
    | Triple of Expression * Operator * Expression

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

let pBind parserProducingFunction parser =
    let innerParser chars =
        match run parser chars with
        | Failure msg -> Failure msg
        | Success (result, remaining) ->
            let parser2 = parserProducingFunction result
            run parser2 remaining
    Parser innerParser

let ( >>= ) parser parserProducerFn = pBind parserProducerFn parser

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



//------------------------HELPER FUNCTIONS-----------------------

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

let flattenTuple nestedTuple = 
    match nestedTuple with
    | ((atom1, operator), atom2) -> (atom1, operator, atom2)

//-------------------------Parsers------------------------------

let whiteCharParser = anyOfParser [' '; '\t']

let whitespaceParser = many whiteCharParser

let digitParser = anyOfParser ['0'..'9']

let digitsParser = many1 digitParser

let decimalParser = opt (charParser '-') .>>. digitsParser |>> signedCharListToDecimal

let alphabetParser = anyOfParser ['a'..'z']

let stringParser = many1 alphabetParser |> pMap charListToString

let operatorParser = 
    anyOfParser ['+'; '-'; '*'; '/'; '='] 
    |>> (fun x ->
            match x with
                | '+' -> Addition("+")
                | '-' -> Subtraction("-")
                | '*' -> Multiplication("*")
                | '/' -> Division("/")
                | '=' -> Equals("="))
        
let variableParser = opt whitespaceParser >>. stringParser .>> opt whitespaceParser |>> (fun x -> Variable(x))

let valueParser = opt whitespaceParser >>. decimalParser .>> opt whitespaceParser |>> (fun x -> Value(x))

let operationParser = operatorParser 

let atomParser = variableParser <|> valueParser |>> (fun x -> Node(x))

//------------------------Expression Parsers-----------------------

let tripleParser = atomParser .>>. operationParser .>>. atomParser |>> flattenTuple |>> (fun x -> Triple(x))

let singleExpressionParser = tripleParser <|> atomParser

let multipleExpressionParser = singleExpressionParser .>>. operatorParser .>>. singleExpressionParser |>> flattenTuple |>> (fun x -> Triple(x))

let expressionParser = multipleExpressionParser <|> singleExpressionParser 

let commandParser = many1 expressionParser

let result1 = run commandParser (stringToCharList " a + 1 + 2 + 2 + 2")

//----------------------INTERPRETER----------------------------

let set (env: Map<string, string>, exp) =
    match exp with
    | Triple (Node(Variable x), Equals "=", Node(Value y)) -> 
        let newEnv = env.Add(x, sprintf "%M" y)
        Success (newEnv, sprintf "%s = %M" x y)
    | _ -> Failure (env, "Error")

let resolve env variable =
    match variable with
    | Node(Variable x) -> Success x
    | _ -> Failure "Error"

let add (env, exp) = 
    match exp with
    | Triple (Node(Value x), Addition "+", Node(Value y)) -> 
        Success (env, sprintf "%M" (x+y))
    | _ -> Failure (env, "Invalid syntax")

let calculate (env, exp) =
    match exp with
    | Triple (_, Addition "+", _) -> add (env, exp)
    | Triple (_, Subtraction "-", _) -> Success (env, "Subtraction")
    | Triple (_, Multiplication "*", _) -> Success (env, "Multiplication")
    | Triple (_, Division "/", _) -> Success (env, "Division")
    | Triple (_, Equals "=", _) -> set (env, exp)
    | Node(Variable x) -> Success (env, x)
    | Node(Value x) -> Success (env, sprintf "%M" x)  
    | _ -> Failure (env, "Invalid input") 

let parse (env, input) = 
    let inputChars = stringToCharList input
    match run expressionParser inputChars with
    | Failure msg -> Failure (env, msg)
    | Success (expression, remaining) ->
        printfn "%A" expression
        Success (env, expression)

let evaluate result =
    match result with
    | Success (env, expression) -> 
        match expression with
        | Node node -> calculate (env, expression)
        | Triple (node1, operator, node2) -> calculate (env, expression)
    | Failure (env, msg) -> Failure (env, msg)
    
let format result =
    match result with
    | Success (env, response) -> env, response
    | Failure (env, errorMsg) -> env, errorMsg 

//-----------------------REPL-----------------------------------

let respond env input =
    let newEnv, response = 
        (env, input)
        |> parse
        |> evaluate
        |> format

    printfn ":> %A" response
    newEnv

let rec repl env =
    printf "<: "
    let newInput = Console.ReadLine()
    let newEnv = respond env newInput
    repl newEnv

let env : Map<string, string> = Map.empty

repl env

