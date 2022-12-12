#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common

let example = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""

let lowercaseA = int32 'a'

let mapPoint x y = function
    | 'S' -> 1
    | 'E' -> 26
    | c -> int32 c - lowercaseA + 1

let getPossibleMoves (map:int[][]) (x, y) = 
    map
    |> Array.neighboursXY x y  
    |> Seq.filter (fun (x', y') -> map[y'][x'] >= map[y][x] - 1)  
    |> List.ofSeq

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

let rec find goal map queue visited =
    match queue with
    | [] -> failwith "Didn't find goal"
    | (node, trace) :: tail -> 
        if goal node then 
            (node :: trace) 
        elif visited |> Set.contains node then
            find goal map tail visited
        else 
            let possibleMoves = getPossibleMoves map node |> List.map (fun x -> (x, node :: trace))

            find goal map (tail @ possibleMoves) (visited |> Set.add node)

let run1 (input:string) = 
    let map, start, finish = parseMap input
    let path = find (fun n -> n = start) map [(finish, [])] Set.empty
    path.Length - 1

let run2 (input:string) = 
    let map, _, finish = parseMap input
    let path = find (fun (x,y) -> map[y][x] = 1) map [(finish, [])] Set.empty
    path.Length - 1    
    
printfn "example"
test <@ example |> run1 = 31 @>
test <@ example |> run2 = 29 @>

printfn "puzzle"
let puzzle = readText "day12";;
test <@ puzzle |> run1 = 517 @>
test <@ puzzle |> run2 = 512 @>