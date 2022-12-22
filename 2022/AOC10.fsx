let input = System.IO.File.ReadLines "/Users/chris/Development/fsharp/AdventOfCode/2022/10.input"

type Instruction = AddX of int | Noop

let instructions =
    input
    |> Seq.map
        (fun line -> 
            if line.StartsWith("noop") then Noop
            else if line.StartsWith("addx") then (int line.[4..] |> AddX)
            else failwithf "Unrecognized instruction: %s" line)

let cycles =
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
                | Noop -> cycle+1, cycles @ [(cycle+1, x')]
            //printfn "X was %d in the last cycle, which was %A. The next cycle is %d. The instruction is %A. During this X will be %d. This produces these cycles: %A" x lastInstruction (cycle+1) instruction x' (List.rev cycles')
            (x', cycle', cycles', instruction))
        (1, 0, [], Noop)
    |> fun (_, _, cycles, _) -> cycles

let cycleMap = cycles |> Map.ofList

let signalStrength (cycle: int) (x: int) = cycle * x

let registerAt (cycleMap: Map<int, int>) cycle =
    cycleMap.[cycle]

let registerAt' = registerAt cycleMap

let signalStrengths = 
    seq { 0..5 }
    |> Seq.map (((*) 40) >> ((+) 20))
    |> Seq.map (fun cycle -> (signalStrength cycle (registerAt' cycle)))

let signalStrengthsSum =
    signalStrengths
    |> Seq.sum

// Part 2

let cycleCount = 
    cycleMap.Keys
    |> Seq.max

for cycle in 1..cycleCount do
    let x = (cycle - 1) % 40
    let printer = if (cycle % 40 = 0) then printfn else printf
    let middlePixel = registerAt' cycle
    if (List.contains x [middlePixel-1; middlePixel; middlePixel+1]) then printer "#" else printer "."
    