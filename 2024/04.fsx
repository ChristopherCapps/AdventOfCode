let input = System.IO.File.ReadAllLines "2024/04.input" 

let lines = input

type Location = int * int
// col, row
type Offset = int * int 

let ltr = Offset(1, 0)
let rtl = Offset(-1, 0)
let ttb = Offset(0, 1)
let btt = Offset(0, -1)
let diagonal_up_right = Offset(1, -1)
let diagonal_down_right = Offset(1, 1)
let diagonal_up_left = Offset(-1, -1)
let diagonal_down_left = Offset(1, -1)

let max_columns = lines.[0].Length
let max_rows = lines.Length

let letter_at (location: Location) =
    let col, row = location
    if col < 0 || row < 0 || col >= max_columns || row >= max_rows then None
    else Some (lines.[row][col])

let letter_after_traversing (offset: Offset) (location: Location) =
    let col, row = (fst location + fst offset), (snd location + snd offset)
    letter_at (col, row)

let found_in_direction (location: Location) (word: string) (offset: Offset) =
    let mutable location' = location
    for letter in word do
        if letter_at location = letter