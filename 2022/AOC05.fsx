let input = System.IO.File.ReadAllLines "/Users/chris/Development/fsharp/AdventOfCode/2022/05.input" 

let stackLines = 
    let stacks = Array.init 9 (fun _ -> [])
    seq { 1..9 }
    |> Seq.fold 
        (fun (stacks: char list list) stackNum -> 
            let stack = 
                seq { 1..8 }
                |> Seq.fold
                    (fun (stack:char list) rowNum ->
                        let rowCoord = 8-rowNum
                        let stackCoord = 1 + (9 - stackNum) * 4
                        let cell = input.[rowCoord].[stackCoord]
                        if (cell <> ' ') then (cell :: stack) else stack)
                    []
            stacks @ [stack])
        []
    |> List.rev
    |> Array.ofList

printfn "%A" stackLines

let stackLines' =  stackLines.Clone() :?> (list<char> array)

let instructions = input |> List.ofArray |> List.skip 10

type Move = 
    | Move of count: int * source: int * target: int

let parseInstruction (instruction: string) =
    let tokens = instruction.Split([|' '|])
    Move(count = int tokens.[1], source = int tokens.[3], target = int tokens.[5])

let pop stack count =
    (List.take count stack, List.skip count stack)

let push stack elements =
    (List.rev elements) @ stack

let getStack index (stacks: char list[]) =
    stacks.[index-1]

let withUpdatedStack index updatedStack (stacks: char list[])  =
    stacks.[index-1] <- updatedStack
    stacks

let apply (pusher: list<'a> -> list<'a> -> list<'a>) stacks (move: Move) =
    let (Move (count, source, target)) = move
    let poppedElements, updatedSource = pop (getStack source stacks) count
    let updatedTarget = pusher (getStack target stacks) poppedElements
    stacks
    |> withUpdatedStack source updatedSource
    |> withUpdatedStack target updatedTarget

let updatedStacks = 
    instructions
    |> Seq.map parseInstruction
    |> Seq.fold (apply push) stackLines

let topOfEachStack =
    updatedStacks
    |> List.ofArray
    |> List.map List.head

// Part 2

let push' stack elements =
    elements @ stack

let updatedStacks' =
    instructions
    |> Seq.map parseInstruction
    |> Seq.fold (apply push') stackLines'

let topOfEachStack' =
    updatedStacks'
    |> List.ofArray
    |> List.map List.head
