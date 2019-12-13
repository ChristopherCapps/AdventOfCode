module AOC06

let input = System.IO.File.ReadAllLines "06.input"

type Body = string

type Orbits = Body * Body

let Root = "COM"

let parseOrbit (line:System.String) =
    let bodies = line.Split([|')'|])
    (bodies.[1], bodies.[0])

let directOrbits = input |> Seq.map parseOrbit |> Map.ofSeq

let pathToCom body =    
    let rec loop body path =
        if body = Root then List.rev ([Root] @ path) else loop (Map.find body directOrbits) [body] @ path
    if body = Root then [] else loop body []

let sumOfOrbits =
    directOrbits
    |> Map.fold (fun sum orbiting _ -> sum + List.length (pathToCom orbiting)) 0 

// PART 2

let pathToYou = pathToCom (Map.find "YOU" directOrbits) |> Set.ofSeq
let pathToSan = pathToCom (Map.find "SAN" directOrbits) |> Set.ofSeq
let commonPath = Set.intersect pathToYou pathToSan
let minimumDistance = (Set.count pathToYou - Set.count commonPath) + (Set.count pathToSan - Set.count commonPath)

// let bodies = 
//     directOrbits
//     |> Seq.fold (fun bodies (Orbit (body1, body2)) -> Set.add body2 (Set.add body1 bodies)) Set.empty

// let directlyOrbits orbited orbiting =
//     directOrbits
//     |> Seq.tryFind (fun (Orbit (orbited', orbiting')) -> orbited = orbited' && orbiting' = orbiting )
//     |> Option.isSome

// // Returns the body that the argument orbits, or None
// let getBodyOrbittedBy body = 
//     directOrbits
//     |> Seq.tryFind (fun (Orbit (_, orbiting)) -> orbiting = body)
//     |> Option.bind (fun (Orbit (orbited, _)) -> Some orbited)

// let orbitChain orbiting =
//     let rec loop orbiting' chain =
//         let directlyOrbits = getBodyOrbittedBy orbiting'
//         match directlyOrbits with
//         | None -> chain
//         | Some directlyOrbited -> [directlyOrbited] @ (loop directlyOrbited chain)
//     loop orbiting []

// // PART 1 answer
// // let bodyChainMap =
// //     bodies
// //     |> Seq.map (fun body -> (body, orbitChain body))
// //     |> Seq.sumBy (fun (body, chain) -> List.length chain)

// // PART 2

// // Returns the body that is orbiting the body argument, or None
// let getBodyOrbitting body =
//     directOrbits
//     |> Seq.tryFind (fun (Orbit (orbited, _)) -> orbited = body)
//     |> Option.bind (fun (Orbit (_, orbiting)) -> Some orbiting)

// let startingBody = getBodyOrbittedBy "YOU"
// let endingBody = getBodyOrbittedBy "SAN"

// let pathsBetween starting ending =
//     let rec loop current ending previousSteps allPaths =
//         if current = ending then (Some previousSteps)
//         else
//             let orbittingBody = getBodyOrbitting current
//             match orbittingBody with
//             | Some body -> 
//                 let orbittingPath = loop orbittingBody ending ([orbittingBody] @ previousSteps) allPaths
//             | None ->  
//                 let orbittedByBody = getBodyOrbittedBy current
//                 match orbittedByBody with
//                 | Some body ->
//                     let orbittedByPath = loop orbittedByBody ending ([orbittedByBody] @ previousSteps) allPaths
//                     match orbittedByPath with
//                     | Some path -> loop orbittedByBody ending ([orbittedByBody] @ previousSteps) (orbittedByPath @ allPaths)
//                     | None -> None
//                 | None -> None
                


