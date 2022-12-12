#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common

let example = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""

type Point = int * int

let lowercaseA = int32 'a'

let mapPoint x y char = 
    let height = 
        match char with
        | 'S' -> 1
        | 'E' -> 26
        | c -> int32 c - lowercaseA + 1

    height

let parseMap (input:string) = 
    let mutable start = (0, 0)
    let mutable finish = (0, 0)

    let map = 
        input.Split("\n") 
        |> Array.mapi (fun y line -> line.ToCharArray() |> Array.mapi (fun x char -> 
            if char = 'S' then start <- (x, y)
            if char = 'E' then finish <- (x, y)
            mapPoint x y char
        ))

    map, start, finish

let getPossibleMoves (map:int[][]) from = 
    let x, y = from
    let currentHeight = map[y][x]

    [ (x, y - 1); (x, y + 1); (x + 1, y); (x - 1, y) ]
    |> List.filter (fun (x', y') -> 
        x' >= 0 && x' < map[0].Length && y' >= 0 && y' < map.Length &&
        map[y'][x'] >= currentHeight - 1
    )    

let rec getPaths (map:int[][]) = 
    [
        for (y, row) in Seq.indexed map do
            for (x, _) in Seq.indexed row do
                let possibleMoves = getPossibleMoves map (x, y)

                for move in possibleMoves do
                    yield ((x, y), move)
    ]
    |> Seq.groupBy fst
    |> Seq.map (fun (from, moves) -> (from, Seq.map snd moves |> List.ofSeq))
    |> Map.ofSeq

let rec find goal (map: Map<Point, Point list>) queue visited =
    match queue with
    | [] -> failwith "Didn't find goal"
    | (node, trace) :: tail -> 
        if goal node then 
            (node :: trace) 
        elif visited |> Set.contains node then
            find goal map tail visited
        else 
            let possibleMoves = 
                match map |> Map.tryFind node with
                | Some(moves) -> moves |> List.map (fun x -> (x, node :: trace))
                | None -> []
            
            find goal map (tail @ possibleMoves) (visited |> Set.add node)

let run1 (input:string) = 
    let map, start, finish = parseMap input

    let pathMap = getPaths map
    let path = find (fun n -> n = start) pathMap [(finish, [])] Set.empty
    path.Length - 1

let run2 (input:string) = 
    let map, _, finish = parseMap input

    let pathMap = getPaths map
    let path = find (fun (x,y) -> map[y][x] = 1) pathMap [(finish, [])] Set.empty
    path.Length - 1    
    

printfn "example"
test <@ example |> run1 = 31 @>
test <@ example |> run2 = 29 @>

printfn "puzzle"
let puzzle = readText "day12";;
test <@ puzzle |> run1 = 517 @>
test <@ puzzle |> run2 = 512 @>