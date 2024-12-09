let input = System.IO.File.ReadAllLines "06.input"

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

let mutable current_heading = HeadingUp

type MoveResult = Obstruction of int*int| Location of int*int | OffMap

let try_move (current_heading: Heading) (current_location: int*int) =
    let move_fn = 
        match current_heading with
        | HeadingUp -> move_up
        | HeadingRight -> move_right
        | HeadingDown -> move_down
        | HeadingLeft -> move_left
    
    let new_location = move_fn current_location
    if is_off_map_at new_location then OffMap
    elif is_obstruction_at new_location then Obstruction new_location
    else Location new_location

let mutable current_location = starting_location
let mutable locations = [starting_location]

while (try_move current_heading current_location) <> OffMap do
    let new_location = try_move current_heading current_location
    match new_location with
    | Obstruction _ -> current_heading <- next_heading current_heading
    | Location (row, col) -> 
        locations <- (row, col) :: locations
        current_location <- (row, col)
    | OffMap -> failwith "Off map"

let distinct_locations = locations |> Set.ofList
distinct_locations |> Set.count

// part 2

let creates_loop_by_obstructing (location: int*int) =
    //printfn "Dropping obstruction at %A" location
    let mutable current_location = starting_location
    let mutable current_heading = HeadingUp
    let mutable obstructions_encountered: (Heading*int*int) list = []
    let mutable loop_created = false
    
    let rec follow_path () =
        //printfn "Current location: %A" current_location
        let next = try_move current_heading current_location
        //printfn "Next location: %A" next
        match next with
        | OffMap -> false
        | Obstruction (row, col) -> 
            let obstruction = (current_heading, row, col)
            if obstructions_encountered |> List.contains obstruction then
                true
            else 
                obstructions_encountered <- obstruction :: obstructions_encountered
                current_heading <- next_heading current_heading
                follow_path()
        | Location (row, col) ->
            if (row, col) = location then
                //printfn "*** Encountered new obstruction at %A" location
                current_heading <- next_heading current_heading
                follow_path()
            else
                current_location <- (row, col)
                follow_path()
    
    follow_path()

// Of all the visited locations, find the ones that will create a loop when obstructed
let loop_locations = 
    distinct_locations
    |> Set.filter ((<>) starting_location)
    |> Set.filter (creates_loop_by_obstructing) 

loop_locations |> Set.count

// loop_locations
// |> Set.toSeq |> Seq.head |> creates_loop_by_obstructing