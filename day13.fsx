#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common
open System.Text.RegularExpressions

let example = """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""

type EntryList = 
    | Value of int
    | List of EntryList list
    with 
        override this.ToString() = 
            match this with
            | Value x -> x.ToString()
            | List x -> "[" + System.String.Join(",",x |> Seq.map (fun x -> x.ToString())) + "]"

let parseEntry (input:string) = 
    let rec parse (input: string list) (acc: EntryList list) = 
        match input with
        | "["::rest -> let list, rest = parse rest [] in parse rest (List list :: acc)
        | "]"::rest -> List.rev acc, rest
        | x::rest -> parse rest (Value (int32 x) :: acc)
        | [] -> acc, []

    // splits on , [ ] but keeps numbers together (ie doesn't split 10 into 1 and 0)
    let inputTokens = (Regex.Split(input, @"([,\[\]])") |> List.ofArray |> List.filter (fun x -> x <> "," && x <> ""))

    parse inputTokens [] |> fst |> List.head

let parseInput (input:string) = 
    // split string ignore empty lines
    input.Split("\n")
    |> Seq.filter (fun x -> x <> "")
    |> Seq.map parseEntry

let compareValues l r = if l < r then Some(true) else if l > r then Some(false) else None

let rec compareEntryLists (l:EntryList, r:EntryList) = 
    match l, r with
    | Value x, Value y -> compareValues x y
    | List x, List y -> 
        let valuesResult = Seq.zip x y |> Seq.tryPick (fun x -> compareEntryLists x)

        match valuesResult with
        | Some(x) -> Some(x)
        | None -> compareValues x.Length y.Length
    | Value x, _ -> compareEntryLists ((List [Value x]),r)
    | _, Value y -> compareEntryLists (l,(List [Value y]))

let comparePair (l:EntryList, r:EntryList) = 
    l, r, compareEntryLists (l,r)

let run1 (input:string) = 
    parseInput input
    |> Seq.chunkBySize 2
    |> Seq.map (fun x -> x.[0], x.[1])
    |> Seq.map comparePair
//    |> Seq.iter (fun (l, r, res) -> printfn "%s <= %s = %A" (string l) (string r) res)
    |> Seq.indexed
    |> Seq.filter (fun (_, (_, _, res)) -> res = Some(true))
    |> Seq.sumBy (fun (i, _) -> i + 1)

let run2 (input:string) = 
    let input = input

    let marker1 = parseEntry "[[2]]"
    let marker2 = parseEntry "[[6]]"

    parseInput input
    |> Seq.append [marker1; marker2]
    |> Seq.sortWith (fun l r -> 
        match compareEntryLists (l,r) with
        | Some(true) -> -1
        | Some(false) -> 1
        | None -> 0
    )
    |> Seq.indexed
    |> Seq.filter (fun (i, x) -> x = marker1 || x = marker2)
    |> Seq.map (fun (i, _) -> i + 1)
    |> Seq.reduce (*)

printfn "example"
// example |> run1
test <@ example |> run1 = 13 @>
test <@ example |> run2 = 140 @>

printfn "puzzle"
let puzzle = readText "day13";;
puzzle |> run1 |> printfn "%A"
puzzle |> run2 |> printfn "%A"