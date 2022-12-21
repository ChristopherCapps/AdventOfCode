let input = System.IO.File.ReadAllText "/Users/chris/Development/fsharp/AdventOfCode/2022/06.input" 

let isDistinctSeq (quartet: char array) =
    let qstr = System.String quartet
    quartet
    |> Seq.map (fun ch -> qstr.Replace(ch, '?'))
    |> Seq.filter (fun qstr' -> qstr'.IndexOf('?') <> qstr'.LastIndexOf('?'))
    |> Seq.isEmpty

let firstDistinctQuartet = 
    input
    |> Seq.windowed 4
    |> Seq.fold 
        (fun (count, found) quartet ->     
            if (Option.isSome found) then (count, found) else if (isDistinctSeq quartet) then (count + 4, Some quartet) else (count + 1, None))
        (0, None)

// Part 2

let firstDistinct14 = 
    input
    |> Seq.windowed 14
    |> Seq.fold 
        (fun (count, found) quartet ->     
            if (Option.isSome found) then (count, found) else if (isDistinctSeq quartet) then (count + 14, Some quartet) else (count + 1, None))
        (0, None)
