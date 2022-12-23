#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common

let example1 = """.....
..##.
..#..
.....
..##.
....."""

let example2 = """....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#.."""

type Grid = {
    Elves: (int*int) Set
    Height: int
    Width: int
}

let x = fst
let y = snd

let (++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let N, S, E, W = (0, -1), (0, 1), (1, 0), (-1, 0)

let NE, SE, SW, NW = N++E, S++E, S++W, N++W

let allDirections = [| N; S; E; W; NE; SE; SW; NW |]

let proposalDirections = [| N; S; W; E |]

let passes = 
    [|
        [| N; NE; NW |]; 
        [| S; SE; SW |]; 
        [| W; NW; SW |]; 
        [| E; NE; SE |];
    |]
    |> Array.map (fun pass -> pass |> Array.map (fun dir -> allDirections |> Array.findIndex (fun a -> a = dir)));

let parseGrid (input:string) = 
    let lines = input.Split("\n")
    let width = lines.[0].Length
    let height = lines.Length

    let elves = 
        seq {
            for y in 0..height-1 do
                for x in 0..width-1 do
                    if lines[y][x] = '#' then
                        yield (x, y)
        } |> Set.ofSeq

    { Elves = elves
      Height = height
      Width = width }

let getProposedMoves (grid:Grid) nextPasses (nextDirections: (int*int) array) = 
    [
        for elf in grid.Elves do
            let empties = 
                allDirections
                |> Array.map (fun dir -> (x elf + x dir, y elf + y dir))
                |> Array.map (fun pos -> grid.Elves |> Set.contains pos |> not)

            //printfn "elf in %A empties %A" elf empties

            // if any are not empty, we can try to move
            if empties |> Seq.exists not then
                let passResults = 
                    nextPasses
                    |> Seq.indexed
                    |> Seq.tryFind (fun (_, pass) -> pass |> Array.forall (fun p -> empties[p]))

                match passResults with
                | Some (i, _) ->
                    //printfn "move proposed for %A with index %i in dir %A" elf i nextDirections[i]
                    let nextDir = nextDirections[i]
                    let proposedMove = (x elf + x nextDir, y elf + y nextDir)
                    yield (elf, proposedMove)
                | None -> ()                  
    ]

let filterProposedMoves (proposedMoves: ((int*int)*(int*int)) list) = 
    proposedMoves
    |> List.groupBy snd
    |> List.choose (fun (_, moves) -> moves |> Seq.tryExactlyOne)

let applyMoves (grid:Grid) (moves: ((int*int)*(int*int)) list) =
    let (elves, count) = 
        ((grid.Elves, 0), moves)
        ||> List.fold (fun (elves, count) (p1, p2) -> (elves |> Set.remove p1 |> Set.add p2, count+1))     

    { grid with Elves = elves }, count

let getGridBounds (grid:Grid) =
    let minY = grid.Elves |> Seq.minBy y |> y
    let maxY = grid.Elves |> Seq.maxBy y |> y
    let minX = grid.Elves |> Seq.minBy x |> x
    let maxX = grid.Elves |> Seq.maxBy x |> x

    (minX, minY, maxX, maxY)

let printGrid (grid:Grid) =
    let (minX, minY, maxX, maxY) = getGridBounds grid

    for y in minY .. 1 .. maxY do
        for x in minX .. 1 .. maxX do
            if grid.Elves |> Set.contains (x, y) then
                printf "#"
            else
                printf "."
        printfn ""

let countEmpty (grid:Grid) =
    let (minX, minY, maxX, maxY) = getGridBounds grid
    let total = (maxX - minX + 1) * (maxY - minY + 1)
    let elves = grid.Elves |> Set.count

    (total - elves)

let processRound (grid: Grid) (n: int) = 
    let nextDirections = [|0..3|] |> Array.map (fun i -> proposalDirections[(i + n) % 4])
    let nextPasses = [|0..3|] |> Array.map (fun i -> passes[(i + n) % 4])
    
    getProposedMoves grid nextPasses nextDirections    
    |> filterProposedMoves
    |> applyMoves grid

let processRounds (grid: Grid) (n: int) = 
    (grid, seq { 0 .. n })
    ||> Seq.fold (fun grid n -> processRound grid n |> fst)

let run1 (rounds: int) (input:string)  = 
    let grid = parseGrid input
    
    processRounds grid (rounds - 1) 
    |> countEmpty

let processUntilHalted (grid: Grid) = 
    let rec loop grid n =
        let (grid, count) = processRound grid n

        if count = 0 then
            (grid, n)
        else
            loop grid (n+1)

    loop grid 0

let run2 (input:string)  = 
    let grid = parseGrid input
    
    processUntilHalted grid 
    |> snd
    |> (+) 1

printfn "example"
example2 |> run1 10 |> printfn "%A"
example2 |> run2 |> printfn "%A"
// test <@ example |> run1 = 1 @>

printfn "puzzle"
let puzzle = readText "day23";;
puzzle |> run1 10 |> printfn "run1: %A"
puzzle |> run2 |> printfn "run2: %A"