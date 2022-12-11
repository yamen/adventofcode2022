#r "nuget: Unquote"
open Swensen.Unquote

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

type Operand = Add | Multiply
type Value = Input | Constant of int64

type Operation = {
  Operand: Operand
  Left: Value
  Right: Value
} with
  member this.Execute input = 
    let getValue value = 
      match value with
        | Input -> input
        | Constant x -> x

    let left = getValue this.Left
    let right = getValue this.Right     

    match this.Operand with
      | Add -> left + right
      | Multiply -> left * right   

let parseOperation (input:string) = 
  let [|left; operand; right|] = input.Split("= ").[1].Split(" ")

  let operand = 
    match operand with 
      | "+" -> Add 
      | "*" -> Multiply 
      | _ -> failwith "invalid operand"

  let getValue = function
    | "old" -> Input
    | x -> Constant(int x)

  let left = getValue(left)
  let right = getValue(right)

  { Operand = operand; Left = left; Right = right }

type Monkey = {
  Id: int
  Operation: Operation
  ReduceWorry: bool
  DivisibilityTest: int64
  IfTrue: int
  IfFalse: int
}

type MonkeyState = {
  mutable Items: int64 list
  mutable InspectionCount: int64
}

type State = {
  MonkeyStates: MonkeyState[]
  Modulus: int64
}

let parseMonkey reduceWorry id (input: string) =
  let lines = input.Split("\n")
  let items = lines.[1].Split(": ").[1].Split(", ") |> Seq.map int64 |> List.ofSeq
  let operation = lines.[2] |> parseOperation
  let divisibilityTest = lines.[3].Split(": ").[1].Split(" ").[2] |> int64
  let ifTrue = lines.[4].Split(": ").[1].Split(" ").[3] |> int
  let ifFalse = lines.[5].Split(": ").[1].Split(" ").[3] |> int

  { Id = id; Operation = operation; DivisibilityTest = divisibilityTest; IfTrue = ifTrue; IfFalse = ifFalse; ReduceWorry = reduceWorry }, {Items = items; InspectionCount = 0}

let parseMonkeys (input: string) reduceWorry =
  input.Split("\n\n") |> Seq.mapi (parseMonkey reduceWorry) |> Array.ofSeq |> Array.unzip

let getTargetMonkey (monkey: Monkey) (worryLevel: int64) =
  if worryLevel % monkey.DivisibilityTest = 0L then
    monkey.IfTrue
  else
    monkey.IfFalse

let processMonkey (state: State) (monkey: Monkey) = 
  let monkeyState = state.MonkeyStates.[monkey.Id]

  for item in monkeyState.Items do
    let newWorryLevel = ((monkey.Operation.Execute item) % state.Modulus) / (if monkey.ReduceWorry then 3L else 1L)
    let targetMonkey = getTargetMonkey monkey newWorryLevel
    state.MonkeyStates.[targetMonkey].Items <- state.MonkeyStates.[targetMonkey].Items @ [newWorryLevel]

  monkeyState.InspectionCount <- monkeyState.InspectionCount + (int64 monkeyState.Items.Length)
  monkeyState.Items <- [];

  state

let processRound (monkeys: Monkey[]) state round = 
  monkeys |> Array.fold processMonkey state

let calculateMonkeyBusiness state =
  state.MonkeyStates 
  |> Array.map (fun x -> x.InspectionCount) 
  |> Array.sortDescending 
  |> Array.take 2 
  |> Array.reduce (fun a x -> a * x)

let run1 rounds reduceWorry (input:string)  =
  let (monkeys: Monkey[]), monkeyStates = parseMonkeys input reduceWorry
  let Modulus = monkeys |> Array.map (fun m -> m.DivisibilityTest) |> Array.reduce (fun a c -> a * c)
  let state = { MonkeyStates = monkeyStates; Modulus = Modulus }

  [1..rounds]
  |> List.fold (processRound monkeys) state
  |> calculateMonkeyBusiness

printfn "example"
test <@ example |> run1 20 true = 10605L @>
test <@ example |> run1 10000 false = 2713310158L @>
// example1.Split("\n") |> run2

printfn "puzzle"
let puzzle = System.IO.File.ReadAllText("data/day11.txt");;
test <@ puzzle |> run1 20 true = 54253 @>
test <@ puzzle |> run1 10000 false = 13119526120L @>