#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common

let example = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""

let parseCoordinate = function 
    | Regex @"(\d+),(\d+)" [x; y] -> (int32 x, int32 y) | _ -> failwith "wtf"

let parsePath (input: string) = 
    input.Split(" -> ") |> Array.map parseCoordinate |> List.ofArray

let parsePaths (input:string) = 
    input.Split("\n") |> Array.map parsePath

type Grid = 
    { grid: char [,]; minX: int; maxX: int; maxY: int }
    with 
        override this.ToString() = 
            let sb = new System.Text.StringBuilder()
            for y in 0..this.maxY do
                for x in this.minX..this.maxX do 
                    sb.Append(this.grid.[y, x]) |> ignore
                sb.AppendLine() |> ignore
            sb.ToString()
        

let buildGrid (addFloorBelow: int) (paths: (int * int) list array) = 
    let maxY = (paths |> Array.map (fun x -> x |> List.map snd |> List.max) |> Array.max) + addFloorBelow

    let minX = (paths |> Array.map (fun x -> x |> List.map fst |> List.min) |> Array.min) 
    let maxX = (paths |> Array.map (fun x -> x |> List.map fst |> List.max) |> Array.max)

    let minX' = if (addFloorBelow > 0) then minX - maxY else minX
    let maxX' = if (addFloorBelow > 0) then maxX + maxY else maxX

    let grid = Array2D.createBased<char> 0 minX' (maxY + 1) (maxX' - minX' + 1) '.'

    for path in paths do
        for i in 0..path.Length-2 do
            let (x1, y1) = path[i]
            let (x2, y2) = path[i+1]
            let (x1, x2) = if x1 < x2 then x1, x2 else x2, x1
            let (y1, y2) = if y1 < y2 then y1, y2 else y2, y1
            for x in x1..x2 do
                for y in y1..y2 do
                    grid[y, x] <- '#'

    if addFloorBelow > 0 then
        for x in minX'..maxX' do
            grid[maxY, x] <- '#'

    { grid = grid; minX = minX'; maxX = maxX'; maxY = maxY }

type SandDropResult = 
    | FallOffEdge
    | StopAt of int * int
    | MoveTo of int * int

let tryMove (x, y) (grid: Grid) = 
    let tryMove boundaryCheck (x',y') =
        if boundaryCheck then Some(FallOffEdge) else
        match grid.grid.[y', x'] with
        | '.' -> Some(MoveTo(x', y'))
        | _ -> None

    let tryMoveDown = tryMove (y >= grid.maxY) (x,y+1)
    let tryMoveLeft = tryMove (x <= grid.minX) (x-1,y+1)
    let tryMoveRight = tryMove (x >= grid.maxX) (x+1,y+1)

    tryMoveDown 
    |> Option.orElse tryMoveLeft 
    |> Option.orElse tryMoveRight 
    |> Option.defaultValue (StopAt(x,y))

let dropSand (x: int, y: int) (grid: Grid) =
    let rec loop (x: int, y: int) (grid: Grid) =
        match tryMove (x, y) grid with
        | MoveTo(x, y) -> loop (x, y) grid
        | StopAt(x, y) -> grid.grid.[y, x] <- 'o'; grid, StopAt(x, y)
        | FallOffEdge -> grid, FallOffEdge

    loop (x, y) grid

let dropSandUntilStopped startPos (grid: Grid) =
    let rec loop (x: int, y: int) sandCount (grid: Grid) =
        let grid', result = dropSand (x, y) grid

        match result with
        | FallOffEdge -> grid, sandCount
        | StopAt(x, y) when (x, y) = startPos -> grid, sandCount
        | _ -> loop startPos (sandCount + 1) grid'

    loop startPos 0 grid

let run1 (input:string) = 
    parsePaths input 
    |> buildGrid 0
    |> dropSandUntilStopped (500,0)
    |> snd

let run2 (input:string) = 
    (parsePaths input 
    |> buildGrid 2
    |> dropSandUntilStopped (500,0)
    |> snd) + 1

printfn "example"
test <@ example |> run1 = 24 @>
test <@ example |> run2 = 93 @>

printfn "puzzle"
let puzzle = readText "day14";;
test <@ puzzle |> run1 = 817 @>
test <@ puzzle |> run2 = 23416 @>