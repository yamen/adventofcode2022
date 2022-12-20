#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common

let example = """2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"""

let parseLine = function | Regex @"(\d+),(\d+),(\d+)" [x; y; z] -> (int32 x, int32 y, int32 z) | _ -> failwith "wtf"
let parseLines (input:string) = input.Split("\n") |> Array.map parseLine

let getAdjacencies (points:array<int*int*int>) = [
        for i in 0..points.Length-1 do
            let (x1, y1, z1) = points[i]
            for j in i+1..points.Length-1 do
                let (x2, y2, z2) = points[j]
                match abs(x1 - x2), abs(y1 - y2), abs(z1 - z2) with
                | 0, 0, 1 
                | 0, 1, 0
                | 1, 0, 0 -> yield (i, j)
                | _ -> ()
    ]

let run1 (input:string) = 
    let points = parseLines input
    let adjacencies = getAdjacencies points

    let totalFaces = points.Length * 6
    let adjacencies = adjacencies |> List.length
    let adjacentFaces = adjacencies * 2
    
    totalFaces - adjacentFaces


printfn "example"
test <@ example |> run1 = 64 @>

// printfn "puzzle"
let puzzle = readText "day18";;
puzzle |> run1 |> printfn "%A"