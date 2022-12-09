let example = """A Y
B X
C Z"""

type Outcome = | Win | Loss | Draw
type Choice = | Rock | Paper | Scissors

let scoreOutcome = function | Win -> 6 | Draw -> 3 | Loss -> 0
let scoreChoice = function | Rock -> 1 | Paper -> 2 | Scissors -> 3
let scoreRound (outcome, choice) = (scoreOutcome outcome) + (scoreChoice choice)

let parseOpponent = function | "A" -> Rock | "B" -> Paper | "C" -> Scissors | _ -> failwith "wtf"
let parseMe1 = function | "X" -> Rock | "Y" -> Paper | "Z" -> Scissors | _ -> failwith "wtf"
let parseMe2 = function | "X" -> Loss | "Y" -> Draw | "Z" -> Win | _ -> failwith "wtf"

let parseRound1 (line: string) = 
    let tokens = line.Split(" ")
    parseMe1 tokens.[1], parseOpponent tokens.[0]

let parseRound2 (line: string) = 
    let tokens = line.Split(" ")
    let opponentChoice = parseOpponent tokens.[0]    
    let targetOutcome = parseMe2 tokens.[1]
    
    let myChoice = 
        match opponentChoice, targetOutcome with
        | a, Draw -> a
        | Rock, Win -> Paper
        | Rock, Lose -> Scissors
        | Paper, Win -> Scissors
        | Paper, Lose -> Rock
        | Scissors, Win -> Rock
        | Scissors, Lose -> Paper

    myChoice, opponentChoice

let processRound (me: Choice, opponent: Choice) =
    let outcome = 
        match me, opponent with
        | Rock, Scissors -> Win
        | Scissors, Paper -> Win
        | Paper, Rock -> Win
        | a, b when a = b -> Draw
        | _ -> Loss

    outcome, me

let run1 (input:string[]) =  
    let sum = 
        input
        |> Array.map parseRound1
        |> Array.map processRound
        |> Array.map scoreRound
        |> Array.sum

    printfn "sum1: %i" sum

let run2 (input:string[]) =  
    let sum = 
        input
        |> Array.map parseRound2
        |> Array.map processRound
        |> Array.map scoreRound
        |> Array.sum

    printfn "sum2: %i" sum    


printfn "example"
example.Split("\n") |> run1
example.Split("\n") |> run2

printfn "puzzle"
System.IO.File.ReadAllLines("data/day2.txt") |> run1
System.IO.File.ReadAllLines("data/day2.txt") |> run2