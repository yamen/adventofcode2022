let example = """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""

type Assignment = { start: int; finish: int}

let parseAssigment (line: string) = 
    let tokens = line.Split("-")
    { start = int32 tokens.[0]; finish = int32 tokens.[1] }

let parsePairs (line: string) = 
    let tokens = line.Split(",")
    parseAssigment tokens.[0], parseAssigment tokens.[1]

let fullyContained (a: Assignment) (b: Assignment) = 
    a.start >= b.start && a.finish <= b.finish

let anyOverlap (a: Assignment) (b: Assignment) = 
    a.start <= b.start && a.finish >= b.start

let run1 (input:string[]) =  
    input 
    |> Array.map parsePairs
    |> Array.filter (fun (a, b) -> fullyContained a b || fullyContained b a)
    |> Array.length

let run2 (input:string[]) =  
    input 
    |> Array.map parsePairs
    |> Array.filter (fun (a, b) -> anyOverlap a b || anyOverlap b a)
    |> Array.length    

printfn "example"
example.Split("\n") |> run1 |> printfn "length1: %i"
example.Split("\n") |> run2 |> printfn "length2: %i"

printfn "puzzle"
let puzzle = System.IO.File.ReadAllLines("data/day4.txt");;
puzzle |> run1 |> printfn "length1: %i"
puzzle |> run2 |> printfn "length2: %i"