module AOC02

let mutable input = (System.IO.File.ReadAllText "02.input").Split([|','|]) |> Array.map int
input.[1] <- 12
input.[2] <- 2

//let input = [|1;9;10;3;2;3;11;0;99;30;40;50|]

type Value = int
type Address = int

type Memory = Value array

type Computation = (Value -> Value -> Value)

type Instruction =
    | Computation of f:Computation * operand1: Value * operand2: Value * address: Address
    | Terminate

let valueAt (memory:Memory) (address:Address) =
    memory.[address]

let dereferencePointer (memory:Memory) (address:Address) =
    valueAt memory (valueAt memory address)

let setValueAt (memory:Memory) (address:Address) value =
    memory.[address] <- value
    memory

let makeComputation f (memory:Memory) (pc:Address) =
    Computation (f, dereferencePointer memory (pc+1), dereferencePointer memory (pc+2), valueAt memory (pc+3))

let decodeInstruction (memory:Memory) pc = 
    let opcode = valueAt memory pc
    match opcode with
    | 1 -> makeComputation (+) memory pc
    | 2 -> makeComputation (*) memory pc
    | 99 -> Terminate
    | _ -> failwithf "Unrecognized opcode: %d" opcode

let rec execute memory pc =
    let instruction = decodeInstruction memory pc
    //printfn "%A" instruction
    match instruction with
    | Computation (f, operand1, operand2, address) -> 
        execute (setValueAt memory address (f operand1 operand2)) (pc+4)
    | Terminate -> memory

for i1 in 0..99 do
    for i2 in 0..99 do
        let memory = Array.copy input
        memory.[1] <- i1
        memory.[2] <- i2
        if (execute memory 0).[0] = 19690720 then printfn "%A" (i1,i2)
