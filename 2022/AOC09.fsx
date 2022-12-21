let input = System.IO.File.ReadLines "/Users/c/Development/AdventOfCode/2022/09.input"  |> Array.ofSeq

type Position = | Position of (int * int)

type Move = | Up of int | Down of int | Left of int | Right of int

let parseMove (line: string) =
    let direction = line.[0]
    let distance = line.[2..]
    match direction with
    | 'U' -> Up (int distance)
    | 'D' -> Down (int distance)
    | 'R' -> Right (int distance)
    | 'L' -> Left (int distance)
    | _ -> failwithf "Unrecognized command: %s" line

let moves =
    input
    |> Seq.map parseMove

let moveHead (move: Move) (Position(headX, headY)) =
    //printfn "Applying head move: %A" move
    match move with 
    | Up y -> 
        seq { 1..y }
        |> Seq.map (fun offset -> Position(headX, headY+offset))
    | Down y -> 
        seq { 1..y }
        |> Seq.map (fun offset -> Position(headX, headY-offset))
    | Left x -> 
        seq { 1..x }
        |> Seq.map (fun offset -> Position(headX-offset, headY))
    | Right x -> 
        seq { 1..x }
        |> Seq.map (fun offset -> Position(headX+offset, headY))

let moveTail (Position(headX, headY)) (Position(tailX, tailY)) =
    let xdiff = (tailX - headX) |> abs
    let ydiff = (tailY - headY) |> abs
    if (xdiff <= 1 && ydiff <= 1) then Position(tailX, tailY)
    else
        match xdiff, ydiff with
        | xdiff, 0 when xdiff >= 2->
            if (tailX > headX) then Position(headX+1, tailY) else Position(headX-1, tailY)
        | 0, ydiff when ydiff >= 2 ->
            if (tailY > headY) then Position(tailX, headY+1) else Position(tailX, headY-1)
        | xdiff, ydiff when xdiff >=2 && ydiff >= 1 && xdiff > ydiff ->
            Position((if (tailX > headX) then (headX+1) else (headX-1)), headY)
        | xdiff, ydiff when xdiff >= 1 && ydiff >= 2 && ydiff > xdiff ->
            Position(headX, if (tailY > headY) then headY+1 else headY-1)
        | _ -> failwithf "Unexpected differential between source %A and target %A" (headX, headY) (tailX, tailY)

let headPositions moves =
    moves
    |> Seq.fold 
        (fun (lastPosition, positions) move -> 
            let newPositions = moveHead move lastPosition
            let newPosition = newPositions |> Seq.rev |> Seq.head
            (newPosition, positions @ (newPositions |> Seq.toList)))
        (Position(0,0), [])
    |> snd

let tailPositions headPositions =
    headPositions
    |> List.fold 
        (fun (lastPosition, positions) head ->
            let position = moveTail head lastPosition
            (position, positions @ [position]))
        (Position(0,0), [])
    |> snd
        
// let tailPositions = 
//     moves
//     |> Seq.fold
//         (fun (headPos, tailPos, visitedPos) move ->
//             let headPositions = moveHead move headPos
//             let tailPositions =
//                 headPositions
//                 |> Seq.fold 
//                     (fun (lastTailPos, tailPositions) headPos -> 
//                         let tailPos' = moveTail headPos lastTailPos
//                         (tailPos', tailPositions @ [tailPos']))
//                     (tailPos, [])
//                 |> snd
//             let headPos' = headPositions |> Seq.rev |> Seq.head
//             let tailPos' = tailPositions |> List.rev |> List.head
//             printfn "Starting positions are %A and %A, moved head to %A and tail to %A with jumps %A" headPos tailPos headPos' tailPos' tailPositions
//             (headPos', tailPos', tailPositions @ visitedPos))
//         (Position(0,0), Position(0,0), [])
//     |> fun (_, _, visitedPos) -> visitedPos

moves |> headPositions |> tailPositions |> List.distinct |> List.length

// Part 2

let headPositions' = moves |> headPositions

let tenthKnotPositions =
    seq { 1..9 }
    |> Seq.fold 
        (fun headPositions knot ->
            let knotPositions = headPositions |> tailPositions
            printfn "Unique knot positions for %d: %d" knot (knotPositions |> List.distinct |> List.length)
            knotPositions)
        headPositions'

let secondKnotPositions = headPositions' |> tailPositions
let thirdKnotPositions = secondKnotPositions |> tailPositions