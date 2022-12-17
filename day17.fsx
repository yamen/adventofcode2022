#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common

let example = """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"""

type Dir = 
    | Lateral of int
    | Down

type Shape = 
    | Horizontal
    | Cross
    | Angle
    | Vertical
    | Square

// from bottom left
let shapePoints = function
    | Horizontal -> [ (0,0); (1,0); (2,0); (3,0) ]
    | Cross -> [ (1,0); (0,1); (1,1); (2,1); (1,2) ]
    | Angle -> [ (0,0); (1,0); (2,0); (2,1); (2,2) ]
    | Vertical -> [ (0,0); (0,1); (0,2); (0,3) ]
    | Square -> [ (0,0); (1,0); (0,1); (1,1) ]

type Grid = {
    Grid: char[, ]
    MaxY: int
    MaxX: int
}

type Rock = {
    Shape: Shape
    Position: int * int
    Points: (int * int) list
}

let parseDir = function | '>' -> Lateral 1 | '<' -> Lateral -1 | _ -> failwith "wtf"
let parseDirs input = input |> Seq.map parseDir |> Seq.collect (fun d -> [d; Down]) |> List.ofSeq

let rockOrder = [| Horizontal; Cross; Angle; Vertical; Square |]
let initGrid height = { Grid = Array2D.create height 7 '.'; MaxY = 0; MaxX = 6 }

let spawnNextRock grid count = 
    let shape = rockOrder[count % 5]

    { Shape = shape; Position = (2, grid.MaxY + 3); Points = shapePoints shape }

let applyRock grid rock c = 
    let mutable newMaxY = grid.MaxY

    for point in rock.Points do
        let (x, y) = (fst rock.Position + fst point, snd rock.Position + snd point)
        grid.Grid[y, x] <- c
        if y >= newMaxY then newMaxY <- y + 1

    { grid with MaxY = newMaxY }

let printGrid grid = 
    for y in grid.MaxY .. -1 .. grid.Grid.GetLowerBound 0 do
        printf "|"
        for x in grid.Grid.GetLowerBound 1 .. 1 .. grid.Grid.GetUpperBound 1 do
            printf "%c" grid.Grid[y, x]
        printfn ""
    printf "|"
    for x in 0 .. grid.Grid.GetUpperBound 1 do printf "_"
    printfn "\n"

let tryMove rock grid dir  = 
    let { Shape = _; Position = (x, y); Points = points } = rock
    let (xt, yt) = match dir with | Lateral x -> (x, 0) | Down -> (0, -1)
    let (x', y') = (x + xt, y + yt)

    let canMove = points |> Seq.forall (fun (xs, ys) -> 
        let (xr, yr) = (x' + xs, y' + ys)
        if xr < 0 || xr > grid.MaxX || yr < 0 then false
        else
            match grid.Grid[yr, xr] with
            | '.' -> true
            | _ -> false
    )

    if canMove then Some { rock with Position = (x', y') } else None

let tickUntil total grid dirsAll = 
    let rec loop rock grid dirs count =

        if count = total then grid else
            match dirs with
            | [] -> loop rock grid dirsAll count
            | dir :: rest ->
                let move = tryMove rock grid dir

                match move, dir with
                | Some rock, _ -> loop rock grid rest count
                | None, Lateral _ -> loop rock grid rest count
                | None, Down ->
                    let grid = applyRock grid rock '#'
                    //printGrid grid
                    loop (spawnNextRock grid (count + 1)) grid rest (count + 1)

    loop (spawnNextRock grid 0) grid dirsAll 0

let run1 (input:string) = 
    let grid = initGrid 5000
    let dirs = parseDirs input
    
    tickUntil 2022 grid dirs |> fun grid -> grid.MaxY

printfn "example"
example |> run1 |> printfn "%A"
test <@ example |> run1 = 3068 @>

// printfn "puzzle"
let puzzle = readText "day17";;
test <@ puzzle |> run1 = 3159 @>