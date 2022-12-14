let example = """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""

let uppercaseA = int32 'A'
let lowercaseA = int32 'a'

let mapItemToPriority (item:char) = 
    if System.Char.IsLower(item) then
        int32 item - lowercaseA + 1
    else
        int32 item - uppercaseA + 27

let mapItems (items:string) = 
    items.ToCharArray() |> Array.map mapItemToPriority |> Set.ofArray

let processRucksack1 (line:string) = 
    let firstHalf = line.[0..line.Length/2-1] |> mapItems
    let secondHalf = line.[line.Length/2..line.Length-1] |> mapItems

    [ firstHalf; secondHalf ]

let processRucksack2 (lines:string[]) = 
    let firstElf = lines.[0] |> mapItems
    let secondElf = lines.[1] |> mapItems
    let thirdElf = lines.[2] |> mapItems

    [ firstElf; secondElf; thirdElf ]

let getCommonItems (sacks) = 
    Set.intersectMany sacks |> Seq.toArray 

let run1 (input:string[]) =  
    input 
    |> Array.map processRucksack1
    |> Array.collect getCommonItems
    |> Array.sum

let run2 (input:string[]) =  
    input 
    |> Array.chunkBySize 3
    |> Array.map processRucksack2
    |> Array.collect getCommonItems
    |> Array.sum    

printfn "example"
example.Split("\n") |> run1 |> printfn "sum1: %i"
example.Split("\n") |> run2 |> printfn "sum2: %i"

printfn "puzzle"
let puzzle = System.IO.File.ReadAllLines("data/day3.txt");;
puzzle |> run1 |> printfn "sum1: %i"
puzzle |> run2 |> printfn "sum2: %i"