#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common

let example = """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."""

type Blueprint = {
    Id: int
    Ore: int
    Clay: int
    Obsidian: int * int
    MaxOre: int
    Geode: int * int
}

type State = {
    Robots: int array
    Ore: int
    Clay: int
    Obsidian: int
    Geode: int
    StepsLeft: int
}

type Action = 
    | BuildOre
    | BuildClay
    | BuildObsidian
    | BuildGeode
    | BuildNothing

let parseBlueprint (input:string) = 
    match input with
    | Regex @"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian." [ blueprint; ore; clay; obsidianOre; obsidianClay; geodeOre; geodeObsidian ] -> 
        { Id = int blueprint; Ore = int ore; Clay = int clay; Obsidian = int obsidianOre, int obsidianClay; Geode = int geodeOre, int geodeObsidian; MaxOre = max (max (int ore) (int clay)) (max (int obsidianOre) (int geodeOre)) }
    | _ -> failwith $"invalid blueprint: {input}"

// this is really not good
// need to have the current max here to determine next actions
let getPossibleActions (state:State) (blueprint:Blueprint) = 
    let maxObsidian = state.Obsidian + (state.StepsLeft * state.Robots[2]) + (state.StepsLeft * (state.StepsLeft + 1) / 2)
    let wantObsidian = maxObsidian > (snd blueprint.Geode) && state.Robots[2] < (snd blueprint.Geode)

    [ 
        if state.Ore >= (fst blueprint.Geode) && state.Obsidian >= (snd blueprint.Geode) then 
            BuildGeode
         
        if wantObsidian then
            if state.Ore >= (fst blueprint.Obsidian) && state.Clay >= (snd blueprint.Obsidian) then 
                BuildObsidian

            if state.Ore >= blueprint.Clay && state.Robots[1] < (snd blueprint.Obsidian) then 
                BuildClay

            if state.Ore >= blueprint.Ore && state.Robots[0] < blueprint.MaxOre then 
                BuildOre

            BuildNothing
        else 
            if state.Ore >= blueprint.Ore && state.Robots[0] < blueprint.MaxOre then 
                BuildOre
            else
                BuildNothing
    ]

let collect (state: State) = 
    let [| ore; clay; obsidian; geode |] = state.Robots
    { state with Ore = state.Ore + ore; Clay = state.Clay + clay; Obsidian = state.Obsidian + obsidian; Geode = state.Geode + geode; StepsLeft = state.StepsLeft - 1}

let addRobot (pos: int) (state: State) = state.Robots |> Array.updateAt pos (state.Robots[pos]+1)

let buildRobot (state: State) (action: Action) (blueprint: Blueprint) = 
    match action with
    | BuildOre -> { state with Ore = state.Ore - blueprint.Ore; Robots = state |> addRobot 0 }
    | BuildClay -> { state with Ore = state.Ore - blueprint.Clay; Robots = state |> addRobot 1 }
    | BuildObsidian -> { state with Ore = state.Ore - (fst blueprint.Obsidian); Clay = state.Clay - (snd blueprint.Obsidian); Robots = state |> addRobot 2 }
    | BuildGeode -> 
        { state with Ore = state.Ore - (fst blueprint.Geode); Obsidian = state.Obsidian - (snd blueprint.Geode); Robots = state |> addRobot 3 }
    | BuildNothing -> state

// let buildRobot (state: State) (action: Action) (blueprint: Blueprint) = 
//     match action with
//     | BuildOre when state.Ore > blueprint.Ore -> { state with Ore = state.Ore - blueprint.Ore; Robots = state |> addRobot 0 }
//     | BuildClay when state.Ore > blueprint.Clay -> { state with Ore = state.Ore - blueprint.Clay; Robots = state |> addRobot 1 }
//     | BuildObsidian when state.Ore > (fst blueprint.Obsidian) && state.Clay > (snd blueprint.Obsidian) -> 
//         { state with Ore = state.Ore - (fst blueprint.Obsidian); Clay = state.Clay - (snd blueprint.Obsidian); Robots = state |> addRobot 2 }
//     | BuildGeode when state.Ore > (fst blueprint.Geode) && state.Obsidian > (snd blueprint.Obsidian) ->
//         { state with Ore = state.Ore - (fst blueprint.Geode); Obsidian = state.Obsidian - (snd blueprint.Geode); Robots = state |> addRobot 3 }
//     | _ -> state

let getMaximumGeodes maxSteps blueprint = 
    let mutable mostGeodes = 0
    let rec loop state = 
        let newState = collect state
        mostGeodes <- max mostGeodes newState.Geode
        let maxGeodes = newState.Geode + (newState.StepsLeft * newState.Robots[3]) + (newState.StepsLeft * (newState.StepsLeft - 1) / 2)
        if maxGeodes < mostGeodes then
            [ state ]
        else if newState.StepsLeft = 0 then 
            [ newState ]
        else
            // use old state for possible actions, as need to build next tick
            match getPossibleActions state blueprint with
            | [] -> loop newState
            | actions -> 
                actions
                |> List.map (fun action -> loop (buildRobot newState action blueprint))
                |> List.concat

    loop ({ Robots = [| 1; 0; 0; 0 |]; Ore = 0; Clay = 0; Obsidian = 0; Geode = 0; StepsLeft = maxSteps })
    |> Seq.maxBy (fun (state) -> state.Geode)

let run1 (input:string) = 
    let blueprints = input.Split("\n") |> Array.map parseBlueprint

    blueprints
    |> Seq.map (fun b -> b.Id, getMaximumGeodes 24 b)
    |> Seq.map (fun (b, s) -> b * s.Geode)
    |> Seq.sum  

printfn "example"
//test <@ example |> run1 = 33 @>
//example |> run2 |> printfn "%A"
// // test <@ example |> run1 = 1 @>

// printfn "puzzle"
let puzzle = readText "day19";;
//puzzle |> run1 |> printfn "%A"
puzzle |> run1 |> printfn "%A"