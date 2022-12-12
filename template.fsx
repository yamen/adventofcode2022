#r "nuget: Unquote"
#r "./Common/bin/Debug/net6.0/Common.dll"
open Swensen.Unquote
open Common

let example = """example"""

let run1 (input:string[]) = 
    1

printfn "example"
example.Split("\n") |> run1
test <@ example.Split("\n") |> run1 = 1 @>

// printfn "puzzle"
// let puzzle = System.IO.File.ReadAllText("data/dayNN.txt");;
// puzzle |> run1