namespace Common

open System.Text.RegularExpressions
open System

[<AutoOpen>]
module Misc = 
    let readLines day = System.IO.File.ReadAllLines($"data/{day}.txt")
    let readText day = System.IO.File.ReadAllText($"data/{day}.txt")
    let inline uncurry2 f a b = f (a, b)
    let min2 (a, b) = uncurry2 min 

[<RequireQualifiedAccess>]
[<AutoOpen>]
module TryParse =
    open System.Text.RegularExpressions

    let private tryParseWith f s =
        match f (string s) with
        | true, v -> Some v
        | _ -> None

    let uint8 s = tryParseWith FSharp.Core.uint8.TryParse s
    let uint16 s = tryParseWith FSharp.Core.uint16.TryParse s
    let uint s = tryParseWith FSharp.Core.uint.TryParse s
    let uint64 s = tryParseWith FSharp.Core.uint64.TryParse s
    let int8 s = tryParseWith FSharp.Core.int8.TryParse s
    let int16 s = tryParseWith FSharp.Core.int16.TryParse s
    let int s = tryParseWith FSharp.Core.int.TryParse s
    let int64 s = tryParseWith FSharp.Core.int64.TryParse s
    let bigint s = tryParseWith FSharp.Core.bigint.TryParse s
    let float32 s = tryParseWith FSharp.Core.float32.TryParse s
    let float s = tryParseWith FSharp.Core.float.TryParse s
    let decimal s = tryParseWith FSharp.Core.decimal.TryParse s
    let regex pattern s =
        let m = Regex.Match(s, pattern)
        if m.Success
        then Some (m.Groups |> Seq.map (fun g -> g.Value) |> Seq.tail |> List.ofSeq)
        else None    

[<AutoOpen>]
module TryParseAPs =
    let (|Uint8|_|) s : uint8 option = TryParse.uint8 s
    let (|Uint16|_|) s : uint16 option = TryParse.uint16 s
    let (|Uint|_|) s : uint option = TryParse.uint s
    let (|Uint64|_|) s : uint64 option = TryParse.uint64 s
    let (|Int8|_|) s : int8 option = TryParse.int8 s
    let (|Int16|_|) s : int16 option = TryParse.int16 s
    let (|Int|_|) s : int option = TryParse.int s
    let (|Int64|_|) s : int64 option = TryParse.int64 s
    let (|BigInt|_|) s : bigint option = TryParse.bigint s
    let (|Float32|_|) s : float32 option = TryParse.float32 s
    let (|Float|_|) s : float option = TryParse.float s
    let (|Decimal|_|) s : decimal option = TryParse.decimal s
    let (|Regex|_|) (pattern : string) s : string list option = TryParse.regex pattern s

    let (|StartsWith|_|) (prefix : string) (s : string) : string option =
        if s.StartsWith prefix
        then Some <| s.Substring(prefix.Length)
        else None         

[<RequireQualifiedAccess>]
[<AutoOpen>]
module Parse =
    open System.Text.RegularExpressions

    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices
    type private Solidify () =
        static member X (o : 'a option, [<CallerMemberName; Optional; DefaultParameterValue("")>] memberName: string) =
            match o with
            | Some v -> v
            | None -> failwith $"{memberName} failed: {o}"

    let uint8 s = s |> TryParse.uint8 |> Solidify.X
    let uint16 s = s |> TryParse.uint16 |> Solidify.X
    let uint s = s |> TryParse.uint |> Solidify.X
    let uint64 s = s |> TryParse.uint64 |> Solidify.X
    let int8 s = s |> TryParse.int8 |> Solidify.X
    let int16 s = s |> TryParse.int16 |> Solidify.X
    let int s = s |> TryParse.int |> Solidify.X
    let int64 s = s |> TryParse.int64 |> Solidify.X
    let bigint s = s |> TryParse.bigint |> Solidify.X
    let float32 s = s |> TryParse.float32 |> Solidify.X
    let float s = s |> TryParse.float |> Solidify.X
    let decimal s = s |> TryParse.decimal |> Solidify.X
    let regex pattern s = s |> TryParse.regex pattern |> Solidify.X    

[<AutoOpen>]       
[<RequireQualifiedAccess>]
module Array =
    let findIndex2D predicate (array: 'a array array) =
        Seq.pick id <| seq {
            for y = 0 to Array.length array - 1 do
                for x = 0 to Array.length array[y] - 1 do
                    if predicate array [y] [x] then yield Some (y, x)
                    else yield None
        }
        
    let neighboursXY x y (array: 'a array array) = seq {
        let deltas = [(0, 1); (0, -1); (1, 0); (-1, 0)]
        for (xd, yd) in deltas do
            let x' = x + xd
            let y' = y + yd
            if y' >= 0 && y' < array.Length &&
                x' >= 0 && x' < array[y'].Length then yield (x', y')
    }

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Array2D =
    let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
        let mutable state = state
        let b1 = Array2D.base1 array
        let b2 = Array2D.base2 array
        for x in b1 .. b1 + Array2D.length1 array - 1 do
            for y in b2 .. b2 + Array2D.length2 array - 1 do
                state <- folder x y state (array.[x, y])
        state
    
    let fold (folder: 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
        let mutable state = state
        let b1 = Array2D.base1 array
        let b2 = Array2D.base2 array
        for x in b1 .. b1 + Array2D.length1 array - 1 do
            for y in b2 .. b2 + Array2D.length2 array - 1 do
                state <- folder state (array.[x, y])
        state    

    let tryGet (v1, v2) (array: 'a [,]) = 
        if v1 >= array.GetLowerBound(0) && v1 <= array.GetUpperBound(0) && v2 >= array.GetLowerBound(1) && v2 <= array.GetUpperBound(1) then
            Some(array[v1, v2])
        else
            None

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Set = 
    let inline sum (source: Set< ^T >) = Set.fold (+) LanguagePrimitives.GenericZero< ^T > source
    let inline length (set: Set<'T>) = set.Count

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Map = 
    let merge m1 m2 = Map.fold (fun acc key value -> Map.add key value acc) m1 m2

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Seq = 
    let splitOn predicate source =
        let mutable i = 0
        source
        |> Seq.groupBy (fun e ->
            if predicate e then i <- i + 1
            i)
        |> Seq.map snd

    let reject predicate source = Seq.filter (predicate >> not) source        

    let splitOnExclusive predicate source =
        let mutable i = 0
        source
        |> Seq.groupBy (fun e ->
            if predicate e then
                i <- i + 1
                -1
            else
                i)
        |> reject (fun (idx, _) -> idx = -1)
        |> Seq.map snd

    let countWhere predicate = Seq.filter predicate >> Seq.length      

[<AutoOpen>]
[<RequireQualifiedAccess>]
module String = 
    let isWhitespaceOrEmpty (str: string) = String.IsNullOrWhiteSpace(str)
    let isEmpty (str: string) = String.IsNullOrEmpty(str)
    let split (separator: string) (str: string) = str.Split(separator)

[<RequireQualifiedAccess>]
[<AutoOpen>]
module Dict =
    open System.Collections.Generic
    open System.Linq

    let ofSeq (source: seq<'k * 'v>) : IDictionary<'k, 'v> =
        source.ToDictionary(fst, snd) :> IDictionary<_, _>

    let containsKey (key: 'k) (dict: IDictionary<'k, 'v>) = dict.ContainsKey(key)

    let find (key: 'k) (dict: IDictionary<'k, 'v>) : 'v =
        match dict.TryGetValue(key) with
        | true, value -> value
        | false, _ ->
            let sampleKeys = dict.Keys |> Seq.truncate 10 |> Array.ofSeq
            failwith $"Couldn't find {key} in dictionary with keys %A{sampleKeys}"

    let tryFind (key: 'k) (dict: IDictionary<'k, 'v>) : 'v option =
        match dict.TryGetValue(key) with
        | true, value -> Some value
        | false, _ -> None    