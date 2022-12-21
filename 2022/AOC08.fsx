let input = System.IO.File.ReadLines "/Users/chris/Development/fsharp/AdventOfCode/2022/08.input"  |> Array.ofSeq

let grid = input

let getVisibility (grid: string array) (x, y) =
    let tree = int grid.[y].[x]
    let horizTrees = grid.[y]
    let vertTreesVisible = 
        grid
        |> Array.map (fun line -> line.[x])
        |> Array.mapi (fun idx otherHeight -> if idx = y then 'T' else if (int otherHeight < tree) then '_' else 'X')
        |> System.String
        
    // visible from any SIDE will count

    let horizTreesVisible =
        horizTrees
        |> Array.ofSeq
        |> Array.mapi (fun idx otherHeight -> if idx = x then 'T' else if (int otherHeight < tree) then '_' else 'X')
        |> System.String

    horizTreesVisible, vertTreesVisible

let isVisible (grid: string array) (x, y) =
    let horizTreesVisible, vertTreesVisible = getVisibility grid (x, y)
    let visibleFromWest = x = 0 || (horizTreesVisible.Substring(0, x).Contains('X') |> not)
    let visibleFromEast = x = grid.[0].Length-1 || (horizTreesVisible.Substring(x+1).Contains('X') |> not)
    let visibleFromNorth = y = 0 || (vertTreesVisible.Substring(0, y).Contains('X') |> not)
    let visibleFromSouth = y = grid.[0].Length-1 || (vertTreesVisible.Substring(y+1).Contains('X') |> not)

    [visibleFromWest; visibleFromNorth; visibleFromEast; visibleFromSouth]

let visibilityGrid: bool array array = Array.init grid.Length (fun y -> Array.init grid.[0].Length (fun x -> false))
for x in 0..(grid.[0].Length-1) do
    for y in 0..(grid.Length-1) do
        visibilityGrid.[x].[y] <- List.contains true (isVisible grid (x, y))

let rowCountVisible row =
    row
    |> Seq.sumBy (fun isVisible -> if isVisible then 1 else 0)

visibilityGrid
|> Seq.sumBy rowCountVisible

// Part 2

let countUnblocked (view: string) =
    let unblockedCount = 
        view
        |> Seq.takeWhile (fun ch -> ch = '_')
        |> Seq.length
    // If we went all the way to the edge, take the count as-is; otherwise, there was another tree in the way, so +1
    if unblockedCount < view.Length then unblockedCount+1 else unblockedCount

let unblockedViews (grid: string array) (x, y) =
    let horizTreesVisible, vertTreesVisible = getVisibility grid (x, y)
    let unblockedToWest = if x = 0 then 0 else (countUnblocked(horizTreesVisible.Substring(0, x).ToCharArray() |> Array.rev |> System.String))
    let unblockedToEast = if x = grid.[0].Length-1 then 0 else (countUnblocked(horizTreesVisible.Substring(x+1)))
    let unblockedToNorth = if y = 0 then 0 else (countUnblocked(vertTreesVisible.Substring(0, y).ToCharArray() |> Array.rev |> System.String))
    let unblockedToSouth = if (y = grid.Length-1) then 0 else (countUnblocked(vertTreesVisible.Substring(y+1)))
    [unblockedToEast; unblockedToNorth; unblockedToSouth; unblockedToWest]

let scenicScore (grid: string array) (x, y) =
    unblockedViews grid (x, y)
    |> List.fold (*) 1


let scores = 
    [for x in 0..(grid.[0].Length-1) do
        for y in 0..(grid.Length-1) do
            yield (scenicScore grid (x, y))]

scores
|> List.max