let input = System.IO.File.ReadAllLines "02.input" |> List.ofArray

let lines = input

// 3 blue, 2 green, 6 red

type Cube = Blue | Red | Green 

type CubeCount = Cube * int

type CubeGroup = CubeCount list

let colorToCube = function
    | "blue" -> Blue
    | "red" -> Red
    | "green" -> Green
    | _ -> failwith "Invalid color"

let parseCubeCount (s: string) =
    let [|count; color;|] = s.Split([|' '|])
    CubeCount(color |> colorToCube, int count)

let parseCubeGroup (s: string) =
    [for cubeCount in s.Split([|','|]) -> parseCubeCount(cubeCount.Trim())]

let parseCubeGame (s: string) =
    [for cubeGroup in s.Split([|';'|]) -> parseCubeGroup(cubeGroup.Trim())]

type Game = int * (list<list<CubeCount>>)

let parseGame(s: string) =
    let [|gameStr: string; gameData: string|] = s.Split([|':'|])
    let gameId = int(gameStr.Substring(5))
    let cubeGame = parseCubeGame gameData
    Game(gameId, cubeGame)

let cubeCountPossible (cube, count) =
    match cube with
        | Blue -> count <= 14
        | Red -> count <= 12
        | Green -> count <= 13

let cubeGroupPossible (group: CubeGroup) =
    (group 
    |> Seq.filter (cubeCountPossible >> not)
    |> Seq.length) = 0

let gamePossible (game: Game) =
    let (id, groups) = game    
    (groups 
    |> Seq.filter (cubeGroupPossible >> not)
    |> Seq.length) = 0
    

let games =
    lines
    |> Seq.map parseGame
    |> Seq.filter gamePossible
    