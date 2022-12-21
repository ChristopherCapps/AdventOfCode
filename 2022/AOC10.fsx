let input = System.IO.File.ReadLines "/Users/c/Development/AdventOfCode/2022/10.input"

type Instruction = AddX of int | Noop

let instructions =
    input
    |> Seq.map
        (fun line -> 
            if line.StartsWith("noop") then Noop
            else if line.StartsWith("addx") then (int line.[4..] |> AddX)
            else failwithf "Unrecognized instruction: %s" line)

let cycleMap =
    instructions
    |> Seq.fold
        (fun (x, cycle, cycles, lastInstruction) instruction ->
            let x' = 
                match lastInstruction with
                | AddX value -> x+value
                | _ -> x
            let cycle', cycles' =
                match instruction with
                | AddX value -> cycle+2, cycles @ [(cycle+1, x'); (cycle+2, x')]
                | Noop -> cycle+1, cycles @ [(cycle+1, x)]
            
            (x', cycle', cycles', instruction))
        (1, 0, [], Noop)
