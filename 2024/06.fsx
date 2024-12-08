let input = System.IO.File.ReadAllLines "2024/06.input"

let lines = input

let starting_location = 
    lines
    |> Array.mapi (fun row line -> 
        let col = line.IndexOf('^')
        if col > -1 then Some (row, col) else None)
    |> Array.filter Option.isSome
    |> Array.head
    |> Option.get

let is_obstruction_at (row, col) =
    lines.[row].[col] = '#'

let is_off_map_at (row, col) =
    row >= lines.Length || col >= lines.[0].Length || row < 0 || col < 0

type Heading = HeadingUp | HeadingDown | HeadingLeft | HeadingRight

let move_up (row, col) = (row-1, col)
let move_down (row, col) = (row+1, col)
let move_left (row, col) = (row, col-1)
let move_right (row, col)= (row, col+1)

let next_heading (heading: Heading) = 
    match heading with
    | HeadingUp -> HeadingRight
    | HeadingRight -> HeadingDown
    | HeadingDown -> HeadingLeft
    | HeadingLeft -> HeadingUp

let current_heading = HeadingUp

type MoveResult = Obstruction | Location of int*int | OffMap
let move (current_heading: Heading) (current_location: int*int) =
    let new_location = 
        match current_heading with
        | HeadingUp -> 