let input = System.IO.File.ReadAllLines "02.input" 

let lines = input

let UP = true
let DOWN = false 

let is_trending (is_increasing: bool) (levels: int array) =
    let (first, second) = if is_increasing then (1, 0) else (0, 1)
    levels 
    |> Array.windowed 2 
    |> Array.map (fun window -> if window.[first] > window.[second] then 0 else 1)
    |> Array.sum
    |> fun sum -> sum = 0

let is_increasing = is_trending UP
let is_decreasing = is_trending DOWN

let is_safe_tolerance level_1 level_2 = 
    let delta = abs (level_1 - level_2)
    delta >= 1 && delta <= 3

let is_safe_report (levels: int array) =
    (is_decreasing levels || is_increasing levels) &&
    levels
    |> Array.windowed 2
    |> Array.map (fun window -> is_safe_tolerance window.[0] window.[1])
    |> Array.reduce (&&)

let reports =
    lines
    |> Array.map (fun line -> line.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun report -> report |> Array.map int)

reports
|> Array.filter (fun report -> is_safe_report report)
|> Array.length

// part 2

let is_dampener_safe (levels: int array) =
    [0..(levels.Length - 1)]
    |> Seq.map (fun idx -> levels |> Array.removeAt idx)
    |> Seq.map is_safe_report
    |> Seq.reduce (||)

reports
|> Array.filter (fun report -> (is_safe_report report) || (is_dampener_safe report))
|> Array.length

