#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common
open System.Collections.Generic

let example = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"""

type Point = int * int

type Grid = {
    Sensors: Point HashSet
    Beacons: Point HashSet
    Searched: Point HashSet
    mutable BoundsX: int * int
    mutable BoundsY: int * int
    Pairs: (Point * Point * int) list
} with
    member this.minX = this.BoundsX |> fst
    member this.maxX = this.BoundsX |> snd
    member this.minY = this.BoundsY |> fst
    member this.maxY = this.BoundsY |> snd

module HashSet = 
    let add (item: 'a) (set: HashSet<'a>) = set.Add(item) |> ignore; set
    let empty<'a> = new HashSet<'a>()

let updateBounds ((x,y): Point) (grid: Grid) = 
    grid.BoundsX <- (min grid.minX x, max grid.maxX x)
    grid.BoundsY <- (min grid.minY y, max grid.maxY y)
    grid

let addPair (sensor: Point) (beacon: Point) (grid: Grid) = 
    let distance = (abs ((fst sensor) - (fst beacon))) + (abs ((snd sensor) - (snd beacon)))
    { grid with 
        Sensors = grid.Sensors |> HashSet.add sensor
        Beacons = grid.Beacons |> HashSet.add beacon
        Pairs = (sensor, beacon, distance) :: grid.Pairs }
    |> updateBounds sensor
    |> updateBounds beacon

let addSearched (searched: Point) (grid: Grid) = 
    if grid.Sensors.Contains searched || grid.Beacons.Contains searched then grid
    else { grid with Searched = grid.Searched |> HashSet.add searched } |> updateBounds searched

let parseLine = function
    | Regex @"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" [xs; ys; xb; yb] -> 
        (int32 xs, int32 ys), (int32 xb, int32 yb)
    | x -> failwith $"wtf {x}"

let buildGrid (beacons: (Point*Point) array) = 
    ({ Sensors = HashSet.empty; Beacons = HashSet.empty; Searched = HashSet.empty; BoundsX = (0,0); BoundsY = (0,0); Pairs = [] }, beacons)
    ||> Array.fold (fun (acc:Grid) (s, b) -> (acc |> addPair s b))

let printGrid (grid: Grid) = 
    for y in grid.minY .. 1 .. grid.maxY do
        printf "%3d " y
        for x in grid.minX .. 1 .. grid.maxX do
            if grid.Sensors.Contains (x, y) then printf "S"
            elif grid.Beacons.Contains (x, y) then printf "B"
            elif grid.Searched.Contains (x, y) then printf "+"
            else printf "."
        printfn ""
    grid

let intersectWithRow (((x, y): Point),_,(distance: int)) (row: int) = 
    // printfn "Checking sensor %A with distance %d against row %d" (x,y) distance row

    if (y + distance) < row || (y - distance) > row then 
        None
    else 
        let amount = abs((abs (y - row)) - distance)
        Some((x - amount, x + amount))

let getRowIntersections (row: int) (grid: Grid) = 
    [
        for pair in grid.Pairs do
            let range = intersectWithRow pair row
            if range.IsSome then yield range.Value
    ]

let countIntersectionsOnRow (grid: Grid) row (intersections: (int*int) list) =
    for (x, y) in intersections do
        for i in [x .. 1 .. y] do
            addSearched (i, row) grid |> ignore

    [ grid.minX .. 1 .. grid.maxX ]
    |> Seq.filter (fun x -> grid.Searched.Contains (x, row))
    |> Seq.length

let tryGetEmptyPointOnRow (grid: Grid) (minCol, maxCol) row = 
    let intersections = grid |> getRowIntersections row |> List.sort

    let rec loop currentMax (intersections: (int*int) list) = 
        match intersections with
        | [] -> None
        | (x1, x2) :: xs -> 
            if x1 > maxCol then None
            elif x1 > currentMax then Some(currentMax+1, row)
            else loop (max currentMax x2) xs

    loop minCol intersections

let run1 (row: int) (input:string) = 
    let beacons = input.Split("\n") |> Array.map parseLine
    let grid = beacons |> buildGrid
    let intersections = grid |> getRowIntersections row

    intersections |> countIntersectionsOnRow grid row

let run2 max (input:string) = 
    let beacons = input.Split("\n") |> Array.map parseLine
    let grid = beacons |> buildGrid

    seq { 0 .. 1 .. max }
    |> Seq.pick (tryGetEmptyPointOnRow grid (0, max))
    |> fun (x,y) -> (
        printfn "Found empty point at %A" (x,y)
        (int64 x) * 4000000L + (int64 y)
    )

printfn "example"
// example |> run1 10
test <@ example |> run1 10 = 26 @>
test <@ example |> run2 20 = 56000011 @>

printfn "puzzle"
let puzzle = readText "day15";;
// puzzle |> run1 2000000 |> printfn "%A"
puzzle |> run2 4000000 |> printfn "%A"