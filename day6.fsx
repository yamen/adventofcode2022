let example = """mjqjpqmgbljsphdztnvjfqwrcgsmlb"""

let run1 windowSize (input:string) = 
    let characters = input.ToCharArray()
    let windows = 
        characters 
        |> Seq.windowed windowSize
        |> Seq.mapi (fun i window -> i, window)

    let i, _ = 
        windows 
        |> Seq.find (fun (i, c) -> 
            (c |> Array.distinct |> Array.length) = windowSize
        )

    i + windowSize

printfn "example"
example|> run1 4 |> printfn "marker: %i"
example|> run1 14 |> printfn "marker: %i"

printfn "puzzle"
let puzzle = System.IO.File.ReadAllText("data/day6.txt");;
puzzle |> run1 4 |> printfn "marker: %i"
puzzle |> run1 14 |> printfn "marker: %i"