module AOC10

let input = System.IO.File.ReadAllLines "2019/AOC2019/10.input"

type Body = | Asteroid

type Coordinate = Coordinate of int * int

type Cell = { coordinate: Coordinate; body: Body option }

let parseCell x y ch = { 
    coordinate = Coordinate (x,y);
    body = 
        match ch with
        | '#' -> Some Asteroid
        | _ -> None
}

let cells = 
    input
    |> Array.mapi (fun y row -> row |> Seq.mapi (fun x ch -> parseCell x y ch) |> Seq.toArray)

let cellAt x y = cells.[y].[x]

type Fraction = Fraction of int * int //rise over run

let slope (Coordinate (x1, y1)) (Coordinate (x2, y2)) =
    Fraction ((y2-y1), (x2-x1))

let gcf a b =
    let n = if a < b then a else b
    if a % n = 0 && b % n = 0 then Some n else // if a or b is the GCF
    seq { 2..n/2} // common GCF has to be no more than half of the lesser of a or b
    |> Seq.rev
    |> Seq.tryFind (fun i -> a%i = 0 && b%i = 0)

let reduce (Fraction (numerator, denominator) as fraction) =    
    match gcf numerator denominator with
    | Some gcf -> Fraction (numerator/gcf, denominator/gcf)
    | None -> fraction

let fractionEquals (f1:Fraction) (f2:Fraction) =
    (reduce f1) = (reduce f2)

