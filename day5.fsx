let example = """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""

let parseCrateLine (line:string) = 
    line.ToCharArray()
    |> Array.chunkBySize 4
    |> Array.map (fun s -> 
        if s.[0] = '[' then 
            s.[1]
        else 
            ' '
    )

let parseStartingCrates (lines:string[]) = 
    let numberOfStacks = lines.[lines.Length-1].Split(' ') |> Array.filter (fun c -> c <> "") |> Array.length

    let stacks = Array.create numberOfStacks []

    let crates = 
        lines 
        |> Seq.map parseCrateLine
        |> Seq.take (lines.Length - 1)
        |> Seq.toArray
        |> Array.rev

    for i in 0..crates.Length-1 do
        for j in 0..numberOfStacks-1 do
            if crates.[i].[j] <> ' ' then
                stacks.[j] <- crates.[i].[j] :: stacks.[j]

    stacks

type Instruction = { amount: int32; source: int32; target: int32 }

let parseInstruction (line:string) = 
    let tokens = line.Split(' ')
    let amount = int32 tokens.[1]
    let source = int32 tokens.[3]
    let target = int32 tokens.[5]

    { amount = amount; source = source; target = target }     

let processInstructions1 (instructions:Instruction[]) (crates:char list[]) =
    for instruction in instructions do
        for i in 1..instruction.amount do
            let crate = crates.[instruction.source-1].Head
            crates.[instruction.source-1] <- crates.[instruction.source-1].Tail
            crates.[instruction.target-1] <- crate :: crates.[instruction.target-1]

let processInstructions2 (instructions:Instruction[]) (crates:char list[]) =    
    for instruction in instructions do
        let mutable tempStack = []
        for i in 1..instruction.amount do
            let crate = crates.[instruction.source-1].Head
            crates.[instruction.source-1] <- crates.[instruction.source-1].Tail
            tempStack <- crate :: tempStack
        for crate in tempStack do
            crates.[instruction.target-1] <- crate :: crates.[instruction.target-1]       

let runWith instructionRunner (input:string[]) = 
    let emptyLine = input |> Array.findIndex (fun s -> s = "")

    let cratesInput, instructionsInput = 
        input 
        |> Array.removeAt emptyLine 
        |> Array.splitAt emptyLine
    
    let crates = parseStartingCrates cratesInput
    let instructions = instructionsInput |> Array.map parseInstruction

    instructionRunner instructions crates

    crates |> Array.map List.head |> System.String

printfn "example"
example.Split("\n") |> runWith processInstructions1 |> printfn "%s"
example.Split("\n") |> runWith processInstructions2 |> printfn "%s"

printfn "puzzle"
let puzzle = System.IO.File.ReadAllLines("data/day5.txt");;
puzzle |> runWith processInstructions1 |> printfn "%s"
puzzle |> runWith processInstructions2 |> printfn "%s"