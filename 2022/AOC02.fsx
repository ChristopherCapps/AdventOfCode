let input = System.IO.File.ReadAllLines "02.input" |> List.ofArray

type Shape = Rock | Paper | Scissors
type Result = Winner | Loser | Draw

let applyRulesFor playerShape opponentShape =
    match playerShape with
    | Rock when opponentShape = Paper -> Loser
    | Rock when opponentShape = Scissors -> Winner
    | Paper when opponentShape = Scissors -> Loser
    | Paper when opponentShape = Rock -> Winner
    | Scissors when opponentShape = Rock -> Loser
    | Scissors when opponentShape = Paper -> Winner
    | _ -> Draw

let roundResultScore = function
    | Winner -> 6
    | Loser -> 0
    | Draw -> 3

let roundShapeScore = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let roundScoreForPlayer playerShape opponentShape =
    applyRulesFor playerShape opponentShape
    |> roundResultScore
    |> (+) (roundShapeScore playerShape)

let mapShape = function
    | 'A' | 'X' -> Rock
    | 'B' | 'Y' -> Paper
    | 'C' | 'Z' -> Scissors
    | _ -> failwith "Illegal play code"

type Round = Shape * Shape

let rounds =
    input
    |> List.map (fun line -> Round ((mapShape line.[0], mapShape line.[2])))

let roundScores =
    rounds
    |> List.map (fun round -> roundScoreForPlayer (snd round) (fst round))

printfn "Total round scores: %A" (List.sum roundScores)

// Part 2

let mapDesiredOutcome = function 
    | 'X' -> Loser
    | 'Y' -> Draw
    | 'Z' -> Winner
    | _ -> failwith "Illegal play code"

let shapeThatLosesAgainst = function
    | Rock -> Scissors
    | Scissors -> Paper
    | Paper -> Rock

let shapeThatWinsAgainst = function
    | Rock -> Paper
    | Scissors -> Rock
    | Paper -> Scissors

let shapeThatDrawsAgainst = id

let shapeForDesiredOutcome outcome opponentShape =
    match outcome with
    | Loser -> shapeThatLosesAgainst opponentShape
    | Winner -> shapeThatWinsAgainst opponentShape
    | Draw -> shapeThatDrawsAgainst opponentShape

type OutcomeRound = Shape * Result

let desiredShapes =
    input
    |> List.map (fun line -> OutcomeRound (mapShape line.[0], mapDesiredOutcome line.[2]))
    |> List.map (fun (opponentShape, desiredOutcome) -> Round (opponentShape, shapeForDesiredOutcome desiredOutcome opponentShape))
    |> List.map (fun (opponentShape, playerShape) -> roundScoreForPlayer playerShape opponentShape)
    |> List.sum

printfn "Desired outcome total: %A" desiredShapes