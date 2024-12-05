let input = System.IO.File.ReadAllLines "2024/04.input" 
//let input = System.IO.File.ReadAllLines "2024/04.input_test" 

let lines = input


type Location = int * int
// col, row
type Offset = Offset of int * int 

let ltr = Offset(1, 0)
let rtl = Offset(-1, 0)
let ttb = Offset(0, 1)
let btt = Offset(0, -1)
let diagonal_up_right = Offset(1, -1)
let diagonal_down_right = Offset(1, 1)
let diagonal_up_left = Offset(-1, -1)
let diagonal_down_left = Offset(-1, 1)

let offsets = [ltr; rtl; ttb; btt; diagonal_up_left; diagonal_up_right; diagonal_down_left; diagonal_down_right]

let max_columns = lines.[0].Length
let max_rows = lines.Length

let letter_at (location: Location) =
    let col, row = location
    if col < 0 || row < 0 || col >= max_columns || row >= max_rows then None
    else Some (lines.[row][col])

let traverse (offset: Offset) (location: Location): Location =
    let (Offset (col_d, row_d)) = offset
    let col, row = location 
    (col + col_d), (row + row_d)

let letter_after_traversing (offset: Offset) (location: Location) =
    let col, row = traverse offset location
    letter_at (col, row)

let found_in_direction (location: Location) (word: string) (offset: Offset) =
    word
    |> Seq.fold (fun (location': Location option) (letter: char) ->     
        location' 
        |> Option.map (fun loc ->
            letter_at loc
            |> Option.map (fun letter' -> 
                if (letter' = letter) then Some (traverse offset loc) else None))
        |> Option.flatten
        |> Option.flatten) 
        (Some location)
    |> Option.isSome

let mutable count = 0

for col in [0..(max_columns-1)] do
    printfn "Column: %d, Count: %d" col count
    for row in [0..(max_rows-1)] do
        let location = (col, row)
        count <- count + 
            (offsets
            |> List.fold (fun count offset ->
                if (found_in_direction location "XMAS" offset) then 
                    printfn "Found in direction %O from location %O" offset location
                    (count+1) else count) 0)

count