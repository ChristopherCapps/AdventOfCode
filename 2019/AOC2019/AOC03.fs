module AOC03

let input = System.IO.File.ReadAllLines "03.input"

type Coordinate = Coordinate of x: int * y: int

type Translation = Coordinate -> Coordinate seq

let translateUp steps (Coordinate(x,y)) =
    seq { for i = 1 to steps do yield Coordinate(x,y+i) }
let translateDown steps (Coordinate(x,y)) = 
    seq { for i = 1 to steps do yield Coordinate(x,y-i) }
let translateRight steps (Coordinate(x,y)) = 
    seq { for i = 1 to steps do yield Coordinate(x+i,y) }
let translateLeft steps (Coordinate(x,y)) = 
    seq { for i = 1 to steps do yield Coordinate(x-i,y) }

let parseTranslation (translation:string) =
    let direction, steps = translation.[0], translation.[1..] |> int
    steps |>
    match direction with
    | 'U' -> translateUp 
    | 'D' -> translateDown 
    | 'R' -> translateRight 
    | 'L' -> translateLeft
    | _ -> failwithf "Unexpected direction in command: %c" direction

let parseLine (line:string) = 
    line.Split([|','|]) 
    |> Seq.map parseTranslation

let lines =
    input
    |> Array.map parseLine

let distance (Coordinate (c1x, c1y)) (Coordinate (c2x, c2y)) =
    System.Math.Abs(c1x - c2x) + System.Math.Abs(c1y - c2y)

let applyTranslation coordinate (translate:Translation) =
    let points = translate coordinate
    let coordinate' = Seq.last points
    (points, coordinate')

let traverse (origin: Coordinate) (translations: Translation seq) =
    translations
    |> Seq.mapFold applyTranslation origin
    |> fst
    |> Seq.concat

let origin = Coordinate (0, 0)

let intersections =
    lines
    |> Seq.map ((traverse origin) >> Set.ofSeq)
    |> Set.intersectMany

let distancesToOrigin =
    intersections
    |> Set.map (fun coord -> (coord, distance origin coord))
    |> Set.toSeq
    |> Seq.sortBy (fun (coord, distance) -> distance)
    |> Seq.head
        
// Part 2

let lineCoords = 
    lines
    |> Seq.map (traverse origin)

let countStepsToCoord coord (line: Coordinate seq) =
    1 + (line |> Seq.findIndex ((=) coord))


// let countStepsToCoord coord (line: Coordinate seq) =
//     let mutable line' = line
//     let mutable steps = 0
//     let mutable found = false    
//     while (not found) and (not Seq.in) do
//         steps <- steps + 1
//         if (Seq.head line') = coord then found <- true
//         line' <- Seq.tail line'
//     steps

let bestSteps = 
    intersections
    |> Set.toSeq
    |> Seq.map (fun coord -> 
        let totalSteps = 
            lineCoords
            |> Seq.sumBy (countStepsToCoord coord)
        (coord, totalSteps))
    |> Seq.sortBy snd
    |> Seq.head