#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common

let example = """1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122"""

let multiplier = function 
    | '0' -> 0L
    | '1' -> 1L
    | '2' -> 2L
    | '-' -> -1L
    | '=' -> -2L
    | _ -> failwith "invalid multiplier"

let calculateSinglePlace (power, char) = (pown 5L power) * (multiplier char)

let calculate (input:string) = 
    let chars = input.ToCharArray()
    let places = Seq.zip (seq { chars.Length - 1 .. -1 .. 0 }) chars

    places
    |> Seq.map calculateSinglePlace
    |> Seq.sum

let getTotalPlaces (target:int64) =
    let rec loop (n:int64) (power:int) =
        let highestNumber = 2L * (pown 5L power)
        if n < highestNumber then power + 1
        else loop n (power + 1)

    loop target 0

let getPlace (target: int64) (place:int) =
    let power = place - 1
    let baseNumber = pown 5L power
    let maxSubsequent = 2L * ((baseNumber - 1L)/4L)

    let character = 
        if target > 0L then
            if target > maxSubsequent then
                if target > (baseNumber + maxSubsequent) then '2' else '1'
            else 
                '0'
        else if target < 0L then
            if target < -maxSubsequent then
                if target < (-baseNumber - maxSubsequent) then '=' else '-'
            else 
                '0'
        else
            '0'
    
    let remainder = target - (calculateSinglePlace (power, character))

    character, remainder

let run1 (input:string) = 
    let target = 
        input.Split("\n")
        |> Seq.map calculate
        |> Seq.sum

    let totalPlaces = getTotalPlaces target

    ((target, []), seq { totalPlaces .. -1 .. 1 })
    ||> Seq.fold (fun (target, chars) place ->
        let char, remainder = getPlace target place
        (remainder, char.ToString() :: chars)
    )
    |> snd
    |> Seq.rev
    |> String.concat ""

printfn "example"
example |> run1 |> printfn "%A"
// test <@ example |> run1 = 1 @>

printfn "puzzle"
let puzzle = readText "day25";;
puzzle |> run1 |> printfn "%A"