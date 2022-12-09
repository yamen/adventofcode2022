let example = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""

let run (input:string[]) =  
    let allElves = 
        input
        |> Array.fold (fun state value -> 
            match state with
            | head :: tail when value <> "" -> head + (int32 value) :: tail
            | _ -> 0 :: state
        ) [ 0 ]

    let topElf = allElves |> List.max
    let sortedElves = allElves |> List.sortDescending
    let topThreeElves = sortedElves |> List.take 3 |> List.sum
    
    printfn "top elf: %i" topElf
    printfn "sorted elves: %s" (sortedElves.ToString())
    printfn "top three elves: %i" topThreeElves


printfn "example"
example.Split("\n") |> run

printfn "puzzle"
System.IO.File.ReadAllLines("data/day1.txt") |> run