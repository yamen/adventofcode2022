#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common

let example = """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3
 
Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"""

let (|Operator|) = function
  | "+" -> (+) 
  | "*" -> (*) 
  | _ -> failwith "invalid operand"  

let parseOperation (input:string) = 
  match input with
  | Regex "new = old (\\+|\\*) (old|\\d+)" [Operator f; right] ->
    match right with
    | "old" -> fun x -> f x x
    | n -> fun x -> f x (int64 n)
  | _ -> failwith $"invalid operation: {input}"

type Monkey = {
  Id: int
  Operation: int64 -> int64
  DivisibilityTest: int64
  IfTrue: int
  IfFalse: int
}

type MonkeyState = {
  mutable Items: int64 list
  mutable InspectionCount: int64
}

type State = MonkeyState[]

let parseMonkey id (input: string) =
  let lines = input.Split("\n")
  let items = lines.[1].Split(": ").[1].Split(", ") |> Seq.map int64 |> List.ofSeq
  let operation = lines.[2] |> parseOperation
  let divisibilityTest = lines.[3].Split(": ").[1].Split(" ").[2] |> int64
  let ifTrue = lines.[4].Split(": ").[1].Split(" ").[3] |> int
  let ifFalse = lines.[5].Split(": ").[1].Split(" ").[3] |> int

  { Id = id; Operation = operation; DivisibilityTest = divisibilityTest; IfTrue = ifTrue; IfFalse = ifFalse }, {Items = items; InspectionCount = 0}

let parseMonkeys (input: string) =
  input.Split("\n\n") |> Seq.mapi parseMonkey |> Array.ofSeq |> Array.unzip

let getTargetMonkey (monkey: Monkey) (worryLevel: int64) =
  if worryLevel % monkey.DivisibilityTest = 0L then
    monkey.IfTrue
  else
    monkey.IfFalse

let processMonkey worryReducer (state: State) (monkey: Monkey) = 
  let monkeyState = state.[monkey.Id]

  for item in monkeyState.Items do
    let newWorryLevel = (monkey.Operation item) |> worryReducer
    let targetMonkey = getTargetMonkey monkey newWorryLevel
    state.[targetMonkey].Items <- state.[targetMonkey].Items @ [newWorryLevel]

  monkeyState.InspectionCount <- monkeyState.InspectionCount + (int64 monkeyState.Items.Length)
  monkeyState.Items <- [];

  state

let processRound worryReducer (monkeys: Monkey[]) state round = 
  monkeys |> Array.fold (processMonkey worryReducer) state

let calculateMonkeyBusiness state =
  state 
  |> Array.map (fun x -> x.InspectionCount) 
  |> Array.sortDescending 
  |> Array.take 2 
  |> Array.reduce (*)

let run rounds worryReducer monkeys state = 
  [1..rounds]
  |> List.fold (processRound worryReducer monkeys) state
  |> calculateMonkeyBusiness  

let run1 rounds (input:string)  =
  let (monkeys: Monkey[]), state = parseMonkeys input
  let worryReducer = fun x -> x / 3L
  run rounds worryReducer monkeys state

let run2 rounds (input:string)  =
  let (monkeys: Monkey[]), state = parseMonkeys input
  let lcm = monkeys |> Array.map (fun m -> m.DivisibilityTest) |> Array.reduce (*)
  let worryReducer = fun x -> x % lcm
  run rounds worryReducer monkeys state

printfn "example"
test <@ example |> run1 20 = 10605L @>
test <@ example |> run2 10000 = 2713310158L @>
// example1.Split("\n") |> run2

printfn "puzzle"
let puzzle = readText "day11";;
test <@ puzzle |> run1 20 = 54253 @>
test <@ puzzle |> run2 10000 = 13119526120L @>