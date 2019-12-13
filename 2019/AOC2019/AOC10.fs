module AOC10

let input = System.IO.File.ReadAllLines "10.input"

type Coordinate = int * int

type Cell = 
    | Asteroid of Coordinate
    | Empty

let parseCell = function
    | '#' -> Asteroid
    | _ -> Empty 

let lines = 
    input
    |> Array.map (Seq.map parseCell >> Seq.toArray)

type Slope = int * int //rise over run

type Offset = 