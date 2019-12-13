module AOC04

let input = seq { 402328..864247 }

let adjacentDigits =
    seq { 0..9 }
    |> Seq.map (fun i -> (string i) + (string i))

let convertToDigits i =
    i
    |> string
    |> Seq.toArray
    |> Array.map (int >> ((+) -48))

let sameAdjacentDigits (number:int) =
    let number' = string number

    adjacentDigits
    |> Seq.exists (fun repeats -> number'.Contains(repeats))

let allDigitsIncrease (number: int) =
    let number' = convertToDigits number
    number'
    |> Array.mapi (fun i _ -> (i = 0) || (number'.[i-1] <= number'.[i]))
    |> Array.exists not
    |> not

let satisfiesAllCriteria i =
    (i |> sameAdjacentDigits) && (i |> allDigitsIncrease)

let count =
    input
    |> Seq.filter satisfiesAllCriteria
    |> Seq.length

// Part 2

let tooManyAdjacentDigits =
    adjacentDigits
    |> Seq.mapi (fun i s -> (s, s + (string i)))

let exactlyTwoAdjacentDigits (number:int) =
    let number' = string number

    tooManyAdjacentDigits
    |> Seq.exists (fun (double, triple) -> number'.Contains(double) && (not (number'.Contains(triple))))

let satisfiesAllCriteria' i =
    (i |> exactlyTwoAdjacentDigits) && (i |> allDigitsIncrease)

let count' =
    input
    |> Seq.filter satisfiesAllCriteria'
    |> Seq.length