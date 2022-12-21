let input = System.IO.File.ReadLines "/Users/chris/Development/fsharp/AdventOfCode/2022/09.input"  |> Array.ofSeq

type Position = | Position of (int * int)

let headPos = Position (0,0)
let tailPos = Position (0,0)

type Move = | Up of int | Down of int | Left of int | Right of int

let parseMove (line: string) =
    match line.[0] with
    | 'U' -> Up (int line.[2])
    | 'D' -> Down (int line.[2])
    | 'R' -> Right (int line.[2])
    | 'L' -> Left (int line.[2])
    | _ -> failwithf "Unrecognized command: %s" line

let moves =
    input
    |> Seq.map parseMove

let moveHead (move: Move) (Position(headX, headY)) =
    match move with 
    | Up y -> Position (headX, headY-y)
    | Down y -> Position (headX, headY+y)
    | Left x -> Position (headX-x, headY)
    | Right x -> Position (headX+x, headY)

let moveTail (Position(headX, headY)) (Position(tailX, tailY)) =
    