module AOC01

let input = System.IO.File.ReadAllLines "01.input"

let fuelFor mass = mass / 3 - 2

let totalFuel =
    input
    |> Seq.sumBy (int >> fuelFor)

let rec recFuelForAll recFuelTotal mass = 
    let fuelForMass = fuelFor mass
    if fuelForMass <= 0 then recFuelTotal
    else recFuelForAll (recFuelTotal+fuelForMass) fuelForMass

let recTotalFuel =
    input
    |> Seq.fold (fun total mass -> recFuelForAll total (int mass)) 0