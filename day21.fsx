#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common
open System.Collections.Generic

let example = """root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"""

type Operation = 
    | Add
    | Subtract
    | Multiply
    | Divide

type MonkeyAction = 
    | Value of int64
    | Op of string * string * Operation

type Monkey = {
    Id: string
    Action: MonkeyAction
}

let either = function
    | Some a, None -> a
    | None, Some b -> b
    | _ -> failwith "invalid either"

let both op = function
    | Some a, Some b -> op a b
    | _ -> failwith "invalid both"

// knows how to fill in the blank if the answer is provided
let eval op (l:int64 option) (r:int64 option) (ans:int64 option) = 
    match ans with
    | None ->
        match op with
        | Add -> both (+) (l, r)
        | Subtract -> both (-) (l, r)
        | Multiply -> both (*) (l, r)
        | Divide -> both (/) (l, r)
    | Some ans ->
        match op with
        | Add -> ans - either (l, r)
        | Multiply -> ans / either (l, r)
        | Subtract -> 
            match l, r with
            | Some l, None -> l - ans
            | None, Some r -> r + ans
            | _ -> failwith "invalid sub"
        | Divide -> 
            match l, r with
            | Some l, None -> l / ans
            | None, Some r -> r * ans
            | _ -> failwith "invalid div"       

let parseMonkey (input:string) =
    let parse (input:string) =
        match input with
        | Regex @"(\d+)" [value] -> Value (int value)
        | Regex @"(\w+) ([\+\-\/\*]) (\w+)" [left; op; right] ->
            Op (left, right, match op with "+" -> Add | "-" -> Subtract | "*" -> Multiply | "/" -> Divide | _ -> failwith $"invalid op: {op}")
        | _ -> failwith $"invalid action: {input}"

    match input with
    | Regex @"(\w+): (.*)" [id; action] -> { Id = id; Action = parse action }
    | _ -> failwith $"invalid monkey: {input}"

let parseMonkeys (input:string) = 
    input.Split("\n") 
    |> Seq.map parseMonkey
    |> Seq.map(fun m -> m.Id, m)
    |> Map.ofSeq

let evaluate monkeys from = 
    let rec loop monkeys monkey = 
        match monkey.Action with
        | Value value -> value
        | Op (left, right, op) -> eval op (Some((loop monkeys (monkeys |> Map.find left)))) (Some((loop monkeys (monkeys |> Map.find right)))) None
    
    loop monkeys (monkeys |> Map.find from)

let run1 (input:string) = 
    let monkeys = parseMonkeys input

    evaluate monkeys "root"

type Path = Left | Right

let find monkeys from target = 
    let rec loop queue = 
        match queue with
        | [] -> None
        | (monkey, path) :: rest ->
            if monkey.Id = target then Some(monkey, path) else
            match monkey.Action with
            | Value _ -> loop rest
            | Op (left, right, _) -> loop (rest @ [(monkeys |> Map.find left), Path.Left :: path; (monkeys |> Map.find right), Path.Right :: path])                

    loop [from, []]
    |> Option.map (fun (_, path) -> path |> List.rev)

let calculate monkeys target from path = 
    let rec loop monkeys target from path = 
        
        let monkey = monkeys |> Map.find from

        match path, monkey.Action with
        | [], _ -> target       
        | Left :: rest, Op (left, right, op) -> 
            let rightValue = evaluate monkeys right
            let targetValue = eval op None (Some(rightValue)) (Some(target))
            loop monkeys targetValue left rest
        | Right :: rest, Op (left, right, op) ->
            let leftValue = evaluate monkeys left
            let targetValue = eval op (Some(leftValue)) None (Some(target))
            loop monkeys targetValue right rest
        | _ -> failwith "invalid path"

    loop monkeys target from path

let run2 (input:string) = 
    let monkeys = parseMonkeys input

    let root = monkeys |> Map.find "root"
    let path = find monkeys root "humn" |> Option.get

    let target, from, path =     
        match path, root.Action with
        | Left :: rest, Op (left, right, _) -> evaluate monkeys right, left, rest
        | Right :: rest, Op (left, right, _) -> evaluate monkeys left, right, rest
        | _ -> failwith "invalid path"

    calculate monkeys target from path

printfn "example"
test <@ example |> run1 = 152L @>
test <@ example |> run2 = 301L @>

printfn "puzzle"
let puzzle = readText "day21";;
test <@ puzzle |> run1 = 364367103397416L @>
test <@ puzzle |> run2 = 3782852515583L @>