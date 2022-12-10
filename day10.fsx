let example1 = """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"""

type Instruction = 
    | Noop
    | AddX of int32

type ScheduledInstruction = { Delay: int32; Instruction: Instruction }
type State = { Cycle: int32; X: int32; SignalStrength: int32 }

let parseInstruction (input: string) = 
    let tokens = input.Split(" ")

    match tokens.[0] with
    | "noop" -> { Delay = 0; Instruction = Noop }
    | "addx" -> { Delay = 1; Instruction = AddX(int32 tokens.[1]) }
    | _ -> failwith "invalid instruction"

let rec processInstructions (state: State) (instructions: ScheduledInstruction list) (stateHistory: State list) =
    let cycle = state.Cycle + 1
    let signalStrength = state.X * cycle

    match instructions with
    | [] -> state, stateHistory
    | head :: tail -> 
        match head.Delay with
        | 0 -> 
            let newState = 
                match head.Instruction with
                | Noop -> state
                | AddX x -> { state with X = state.X + x }

            processInstructions { newState with Cycle = cycle; SignalStrength = signalStrength } (tail) (state :: stateHistory)
        | _ -> 
            processInstructions { state with Cycle = cycle; SignalStrength = signalStrength } ({ head with Delay = head.Delay - 1 } :: tail) (state :: stateHistory)

let run1 (targetCycles: int32[]) (input:string[])  = 
    let instructions = input |> Seq.map parseInstruction |> Seq.toList

    let state: State = { Cycle = 0; X = 1; SignalStrength = 0 }
    let _, history = processInstructions state instructions []

    let matchingCycles = history |> List.filter (fun state -> Array.contains state.Cycle targetCycles)

    matchingCycles |> List.sumBy (fun state -> state.SignalStrength)

let run2 (input:string[])  = 
    let instructions = input |> Seq.map parseInstruction |> Seq.toList

    let state: State = { Cycle = 0; X = 1; SignalStrength = 0 }
    let _, history = processInstructions state instructions []

    history
    |> Array.ofList |> Array.rev
    |> Array.map (fun state -> ((state.Cycle) % 40, state))
    |> Array.map (fun (pixel, state) -> 
        if abs (state.X - pixel) <= 1 then "#" else "."
    )
    |> Array.chunkBySize 40
    |> Array.map (String.concat "")

printfn "example"
//example1.Split("\n") |> run1 [| 20; 60; 100; 140; 180; 220 |] |> printfn "signal: %i"
example1.Split("\n") |> run2

printfn "puzzle"
let puzzle = System.IO.File.ReadAllLines("data/day10.txt");;
// puzzle |> run1 [| 20; 60; 100; 140; 180; 220 |] |> printfn "signal: %i"
puzzle |> run2