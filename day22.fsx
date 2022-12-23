#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common
open System.Text.RegularExpressions

let example = """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"""

type Turn = L | R
type Instruction = Move | Turn of Turn
type Bearing = | E = 0 | S = 1 | W = 2 | N = 3

let bearings = [| (1,0); (0, 1); (-1, 0); (0, -1) |]
let getMoveFromBearing (bearing: Bearing) = bearings[int32 bearing]

let turn (bearing: Bearing) (turn: Turn) =
    match turn with
    | L -> enum<Bearing> ((int32 bearing + 3) % 4)
    | R -> enum<Bearing> ((int32 bearing + 1) % 4)

let parseInstructions (input:string) = 
    [
        for m in Regex.Split(input, @"([RL])") do
            match m with
            | "R" -> yield Turn R
            | "L" -> yield Turn L
            | s -> yield! Array.create (int s) Move
    ]

let parseMap (rows:string[]) = 
    let width = rows |> Array.maxBy (fun r -> r.Length) |> String.length
    let height = rows.Length

    let map = Array2D.zeroCreate height width

    for y in 0..height-1 do
        let lineWidth = rows.[y].Length
        for x in 0..width-1 do
            if x < lineWidth then
                map[y, x] <- rows.[y].[x]
            else
                map[y, x] <- ' '

    map

let parseInput (input:string) = 
    let split = input.Split("\n")
    let instructions = split |> Array.last
    let map = split.[0 .. split.Length - 2]

    parseMap map, parseInstructions instructions

let printMap (map:char[,]) pos =
    for y in 0..map.GetLength(0)-1 do
        for x in 0..map.GetLength(1)-1 do
            if pos = (x,y) then printf "X" else printf "%c" map.[y, x]
        printfn ""    

let findWraparound (x, y) (bearing:Bearing) (map:char[,]) = 
    let rec loop (x, y) mover = 
        match map[y, x] with
        | ' ' -> loop (mover (x, y)) mover
        | c -> x, y

    match bearing with
    | Bearing.E -> loop (0, y) (fun (x, y) -> x + 1, y)
    | Bearing.S -> loop (x, 0) (fun (x, y) -> x, y + 1)
    | Bearing.W -> loop (map.GetLength(1) - 1, y) (fun (x, y) -> x - 1, y)
    | Bearing.N -> loop (x, map.GetLength(0) - 1) (fun (x, y) -> x, y - 1)
    | _ -> failwith "invalid bearing"

let moveBy (x, y) (x', y') = (x + x', y + y')

let tryMoveBy pos bearing (map: char[,]) = 
    let x, y = pos
    let x', y' = moveBy (x, y) (getMoveFromBearing bearing)

    let x', y' = 
        if x' < 0 || y' < 0 || x' >= map.GetLength(1) || y' >= map.GetLength(0) || map[y', x'] = ' ' then
            findWraparound pos bearing map
        else 
            x', y'

    if map.[y', x'] = '#' then
        pos
    else
        x', y'

let explore pos bearing map instructions = 
    let rec loop pos bearing instructions map = 
        match instructions with
        | Move :: rest -> 
            let move = (tryMoveBy pos bearing map)
            loop move bearing rest map
        | Turn t :: rest -> 
            loop pos (turn bearing t) rest map
        | [] -> pos, bearing

    loop pos bearing instructions map

let run1 (input:string) = 
    let map, instructions = parseInput input
    
    let (x, y), bearing = 
        instructions
        |> explore (0, 0) Bearing.E map

    1000 * (y+1) + 4 * (x+1) + (int32 bearing)

printfn "example"
test <@ example |> run1 = 6032 @>

printfn "puzzle"
let puzzle = readText "day22";;
puzzle |> run1