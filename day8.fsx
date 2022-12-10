let example = """30373
25512
65332
33549
35390"""

let parseTreeGrid (input:string[]) = 
    input
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int32))

let isTreeVisibleByRow (treeGrid: int32[][]) r c tree =
    let rec loop col fromRight =
        if col = c then
            true
        else
            if treeGrid.[r].[col] >= tree then
                false
            else
                if fromRight then loop (col - 1) fromRight else loop (col + 1) fromRight

    // check from left and from right
    loop 0 false || loop (treeGrid.[0].Length - 1) true

let isTreeVisibleByColumn (treeGrid: int32[][]) r c tree =
    let rec loop row fromBottom =
        if row = r then
            true
        else
            if treeGrid.[row].[c] >= tree then
                false
            else
                if fromBottom then loop (row - 1) fromBottom else loop (row + 1) fromBottom

    // check from top and from bottom
    loop 0 false || loop (treeGrid.Length - 1) true    

let isTreeVisible (treeGrid: int32[][]) r c tree =
    if r = 0 || c = 0 || r = treeGrid.Length - 1 || c = treeGrid.[0].Length - 1 then 
        true
    else
        isTreeVisibleByRow treeGrid r c tree || isTreeVisibleByColumn treeGrid r c tree

let treeVisibilityLeft (treeGrid: int32[][]) r c tree =   
    let rec loop col visibility =
        if col = 0 || treeGrid.[r].[col] >= tree then
            visibility + 1
        else
            loop (col - 1) (visibility + 1)

    loop (c - 1) 0

let treeVisibilityRight (treeGrid: int32[][]) r c tree =   
    let rec loop col visibility =
        if col = treeGrid.[0].Length - 1 || treeGrid.[r].[col] >= tree then
            visibility + 1
        else
            loop (col + 1) (visibility + 1)

    loop (c + 1) 0   

let treeVisibilityUp (treeGrid: int32[][]) r c tree =   
    let rec loop row visibility =
        if row = 0 || treeGrid.[row].[c] >= tree then
            visibility + 1
        else
            loop (row - 1) (visibility + 1)

    loop (r - 1) 0     

let treeVisibilityDown (treeGrid: int32[][]) r c tree =   
    let rec loop row visibility =
        if row = treeGrid.Length - 1 || treeGrid.[row].[c] >= tree then
            visibility + 1
        else
            loop (row + 1) (visibility + 1)

    loop (r + 1) 0      

let getTreeVisibility (treeGrid: int32[][]) r c tree =
    if r = 0 || c = 0 || r = treeGrid.Length - 1 || c = treeGrid.[0].Length - 1 then 
        0
    else 
        treeVisibilityLeft treeGrid r c tree * 
        treeVisibilityRight treeGrid r c tree * 
        treeVisibilityUp treeGrid r c tree * 
        treeVisibilityDown treeGrid r c tree

let processTreeGrid processor (treeGrid: int32[][]) =
    treeGrid
    |> Array.mapi (fun r row -> 
        row |> Array.mapi (fun c tree -> 
            processor treeGrid r c tree
        )
    )

let run1 (input:string[]) = 
    input
    |> parseTreeGrid
    |> processTreeGrid isTreeVisible
    |> Array.sumBy (fun row -> row |> Array.sumBy (fun visible -> if visible then 1 else 0))

let run2 (input:string[]) = 
    input
    |> parseTreeGrid
    |> processTreeGrid getTreeVisibility
    |> Array.map (fun row -> row |> Array.max)
    |> Array.max

printfn "example"
example.Split("\n") |> run1 |> printfn "visible: %i"
example.Split("\n") |> run2 |> printfn "visibility: %i"

printfn "puzzle"
let puzzle = System.IO.File.ReadAllLines("data/day8.txt");;
puzzle |> run1 |> printfn "visible: %i"
puzzle |> run2 |> printfn "visibility: %i"