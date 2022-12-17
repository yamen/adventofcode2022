#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common

let example = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"""

type Valve = {
    Id: string
    FlowRate: int64
    Paths: string list
}

let parseValve (input:string) = 
  match input with
  | Regex @"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)$" [id; flowRate; path] ->
    { Id = id; FlowRate = int64 flowRate; Paths = path |> String.split ", " |> List.ofSeq }
  | _ -> failwith $"invalid valve: {input}"

let findValve valves identity = valves |> List.find (fun v -> v.Id = identity)

let getPaths (startValve: Valve) (valves: Valve list) =
    let significantValves = valves |> List.filter (fun v -> v.FlowRate > 0) |> Set.ofSeq

    let rec getDistance (level : Set<Valve>) (target : Valve) (depth : int64) =
        if level.Contains target then
            depth
        else
            let nextLevel = level |> Seq.collect (fun v -> v.Paths |> Seq.map (findValve valves)) |> Set.ofSeq
            getDistance nextLevel target (depth + 1L)

    significantValves
    |> Set.add startValve
    |> Seq.map (fun source ->
        source,
        significantValves
        |> Seq.filter (fun target -> target <> source)
        |> Seq.map (fun target -> target, getDistance (Set.singleton source) target 0L)
        |> Map.ofSeq)
    |> Map.ofSeq

let makeGraphviz (valves: Valve list) (paths: Map<Valve, Map<Valve, int64>>) =
    let nodes = valves |> Seq.map (fun v -> v.Id, v.FlowRate)
    let edges = paths |> Seq.collect (fun (KeyValue (f, v)) -> v |> Seq.map (fun (KeyValue (t, l)) -> f.Id, t.Id, l))

    let node (id, flowRate) = 
        if flowRate = 0L then
            $"""{id} [label="{id} ({flowRate})" color=red]"""
        else
            $"""{id} [label="{id} ({flowRate})"]"""

    let edge (s, t, l) = $"""{s} -> {t} [label="{l}"]"""

    $"""digraph {{
    {nodes |> Seq.map node |> String.concat ";\n"} 
    {edges |> Seq.map edge |> String.concat ";\n"}
}}"""

type State = {
    TimeLeft: int64
    PressureReleased: int64
    Current: Valve
    Released: Set<string>
    Path: string list
}

let calculatePaths totalTime startValve (paths: Map<Valve, Map<Valve, int64>>) = 
    let mutable best = { TimeLeft = totalTime; PressureReleased = 0; Current = startValve; Released = Set.empty; Path = [] }

    let rec calculatePathsFromNode state = 
        if state.PressureReleased > best.PressureReleased then
            best <- state

        if state.TimeLeft > 0 then
            if state.Released.Contains(state.Current.Id) then
                paths
                |> Map.find state.Current
                // now calculating all possible paths from each productive node to each other node
                // so no need to allow backtracking
                |> Map.filter (fun v _ -> not (state.Released.Contains v.Id))
                |> Seq.iter (fun (KeyValue (v, d)) -> calculatePathsFromNode { state with TimeLeft = state.TimeLeft - d; Current = v; Path = $"Move to {v.Id}" :: state.Path })
            else
                calculatePathsFromNode { state with TimeLeft = state.TimeLeft - 1L; PressureReleased = state.PressureReleased + (state.Current.FlowRate * state.TimeLeft); Released = state.Released.Add(state.Current.Id); Path = $"Release {state.Current.Id}" :: state.Path }

    calculatePathsFromNode best

    { best with Path = best.Path |> List.rev }
        
let run1 (input:string) = 
    let valves = input.Split("\n") |> Seq.map parseValve |> List.ofSeq
    let startValve = findValve valves "AA"
    let paths = getPaths startValve valves

    printfn "%A" (makeGraphviz valves paths)

    calculatePaths 30 startValve paths

printfn "example"
example |> run1 |> printfn "%A"
// test <@ example |> run1 = 1 @>

// printfn "puzzle"
let puzzle = readText "day16";;
puzzle |> run1 |> printfn "%A"