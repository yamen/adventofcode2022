#r "nuget: Unquote"
open Swensen.Unquote

let example = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""

let exampleLong = """R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"""

type Direction = R | U | L | D
type Instruction = { Direction: Direction; Distance: int32 }

let followOnAxis (leader: int32) (follower: int32) = follower + leader.CompareTo(follower)

type Point = { X: int32; Y: int32 } with
    member this.Move (direction: Direction) = 
        match direction with
        | R -> { X = this.X + 1; Y = this.Y }
        | U -> { X = this.X; Y = this.Y + 1 }
        | L -> { X = this.X - 1; Y = this.Y }
        | D -> { X = this.X; Y = this.Y - 1 }

    member this.Touching other = abs (this.X - other.X) <= 1 && abs (this.Y - other.Y) <= 1

    member this.Follow other = 
        if this.Touching other then
            this
        else
            { X = followOnAxis other.X this.X; Y = followOnAxis other.Y this.Y }

type Rope = Point list

type State = { Rope: Rope; Instructions: Instruction list; TailVisited: Point list }

let createPoint = { X = 0; Y = 0 }
let createRope length = List.init length (fun _ -> createPoint)
let getLastPoint rope = rope |> List.last

let parseInstruction (input: string) = 
    let tokens = input.Split(" ")

    let direction = 
        match tokens.[0] with
        | "R" -> R | "U" -> U | "L" -> L | "D" -> D
        | _ -> failwith "invalid direction"

    let distance = int32 tokens.[1]

    { Direction = direction; Distance = distance }

let printRope (rope: Rope) = 
    let maxX = rope |> List.map (fun p -> p.X) |> List.max
    let minX = rope |> List.map (fun p -> p.X) |> List.min
    let maxY = rope |> List.map (fun p -> p.Y) |> List.max
    let minY = rope |> List.map (fun p -> p.Y) |> List.min

    let ropeArray = rope |> Array.ofList

    // cycle from 'top to bottom'
    for y in maxY .. -1 .. minY  do
        // and left to right
        for x in minX .. 1 .. maxX do        
            match ropeArray |> Array.tryFindIndex (fun p -> p.X = x && p.Y = y) with
            | Some(0) -> printf "H"
            | Some(i) when i = ropeArray.Length - 1 -> printf "T"
            | Some(i) -> printf "%i" i
            | None -> printf "."
        printfn ""

let moveRope (rope: Rope) (direction: Direction) =
    let newHead = rope.Head.Move direction

    let newTail = 
        rope.Tail
        |> Seq.scan (fun leader follower -> follower.Follow leader) newHead
        |> Seq.skip 1
        |> List.ofSeq

    newHead :: newTail

let rec processSteps (state: State) printSteps = 
    match state.Instructions with
    | [] -> state
    | next::rest ->
        let newRope = moveRope state.Rope next.Direction
        
        if printSteps then
            printfn ""
            printRope newRope

        let newInstructions = 
            match next.Distance with
            | 1 -> rest
            | d -> { Direction = next.Direction; Distance = d - 1 } :: rest

        processSteps { Rope = newRope; Instructions = newInstructions; TailVisited = (getLastPoint newRope) :: state.TailVisited } printSteps

let run1 ropeSize printSteps (input:string[]) = 
    let instructions = 
        input
        |> Seq.map parseInstruction
        |> List.ofSeq

    let rope = createRope ropeSize

    let finalState = processSteps { Rope = rope; Instructions = instructions; TailVisited = [ getLastPoint rope ] } printSteps

    finalState.TailVisited |> Set.ofList |> Set.count

printfn "example"
test <@ example.Split("\n") |> run1 2 false = 13 @>
test <@ example.Split("\n") |> run1 10 false = 1 @>
test <@ exampleLong.Split("\n") |> run1 10 false = 36 @>

printfn "puzzle"
let puzzle = System.IO.File.ReadAllLines("data/day9.txt");;
puzzle |> run1 2 false |> printfn "visited: %i"
puzzle |> run1 10 false |> printfn "visited: %i"