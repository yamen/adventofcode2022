let example = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""
    

let changeDirectory (directory: string) (command: string) = 
    let tokens = command.Split(" ") |> List.ofArray

    match tokens with
    | [ "$"; "cd"; "/" ] -> "/"
    | [ "$"; "cd"; ".." ] ->
        let elements = directory.Split("/")
        (elements.[0..(elements.Length - 3)] |> String.concat "/") + "/"
    | [ "$"; "cd"; path ] -> directory + path + "/"
    | _ -> failwithf "Invalid change directory command: %s" command

let getDirectorySize ((directory: string), (files: string list)) =
    let size = 
        files
        |> List.map (fun file -> 
            let tokens = file.Split(" ")
            int32 tokens.[0])
        |> List.sum

    directory, size

let getDirectorySizes (lines: string[]) =
    lines
    |> Array.filter (fun line -> not(line.StartsWith("$ ls") || line.StartsWith("dir ")))
    // keep track of current directory and running list of files per directory
    |> Array.scan (fun (currentDir, files) line -> 
        if line.StartsWith("$ cd") then 
            (changeDirectory currentDir line, []) 
        else
            (currentDir, line :: files)) 
        ("/", [])
    // collapse file list into size per directory
    |> Array.map getDirectorySize
    // group by directory name and just keep the latest
    |> Array.groupBy fst
    |> Array.map (fun (_, files) -> files |> Array.maxBy snd)

let getChildDirectorySize (directory: string) (directorySizes: (string * int32)[]) =
    directorySizes
    |> Array.filter (fun (dir, _) -> dir.StartsWith(directory) && dir <> directory)
    |> Array.map snd
    |> Array.sum

let getDeepDirectorySizes (directorySizes: (string * int32)[]) =
    directorySizes
    |> Array.map (fun (dir, size) -> 
        dir, (getChildDirectorySize dir directorySizes) + size
    )

let run1 (input:string[]) = 
    if input.[0] <> "$ cd /" then failwith "Currently only support commands that start at root directory"

    input 
    |> Array.skip 1 
    |> getDirectorySizes 
    |> getDeepDirectorySizes
    |> Array.filter (fun (_, size) -> size <= 100000)
    |> Array.sumBy snd

let run2 (input:string[]) = 
    if input.[0] <> "$ cd /" then failwith "Currently only support commands that start at root directory"

    let totalSize = 70000000
    let neededSize = 30000000

    let allDirectories = 
        input 
        |> Array.skip 1 
        |> getDirectorySizes 
        |> getDeepDirectorySizes

    let unusedSize = totalSize - (allDirectories.[0] |> snd)
    let needToDelete = neededSize - unusedSize
    
    allDirectories
    |> Array.sortBy snd
    |> Array.find (fun (dir, size) -> size >= needToDelete)
    |> snd    

printfn "example"
example.Split("\n") |> run1 |> printfn "size: %i"
example.Split("\n") |> run2 |> printfn "delete: %i"

printfn "puzzle"
let puzzle = System.IO.File.ReadAllLines("data/day7.txt");;
puzzle |> run1 |> printfn "size: %i"
puzzle |> run2 |> printfn "delete: %i"