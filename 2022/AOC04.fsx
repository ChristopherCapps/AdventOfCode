let input = System.IO.File.ReadAllLines "04.input" |> List.ofArray

type Range = int * int

let hasOverlapInner r1 r2 =
    ((fst r1) >= (fst r2) && (snd r1) <= (snd r2))

let hasOverlap (r1:Range, r2:Range) = (hasOverlapInner r1 r2) || (hasOverlapInner r2 r1)

let parseLine (line:string) =
    let ranges = line.Split([|','|])
    let r1 = ranges.[0].Split([|'-'|])
    let r2 = ranges.[1].Split([|'-'|])
    (Range(int r1.[0], int r1.[1]), Range(int r2.[0], int r2.[1]))

let countOverlaps =
    input
    |> List.map parseLine
    |> List.filter hasOverlap
    |> List.length

printfn "%A" countOverlaps

// Part 2

let anyOverlapInner (r1:Range, r2:Range) =
    ((fst r1) >= (fst r2) && (fst r1) <= (snd r2))

let anyOverlap (r1:Range, r2:Range) =
    (anyOverlapInner (r1, r2)) || (anyOverlapInner (r2, r1))

let countAnyOverlaps =
    input
    |> List.map parseLine
    |> List.filter anyOverlap
    |> List.length

printfn "%A" countAnyOverlaps