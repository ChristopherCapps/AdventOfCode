module AOC08

let input = System.IO.File.ReadAllText "08.input"

let digits = input |> Seq.map (fun ch -> (int ch) - 48) |> Seq.toArray

let Width, Height = 25, 6

type Pixel = int

type Line = int[]

type Layer = int[][]

let t
let lines = digits |> Array.chunkBySize Width

let layers = lines |> Array.chunkBySize Height

let pixelsPerLayer pixel (layer:Layer) =
    layer
    |> Array.sumBy (Array.sumBy (fun pixel' -> if pixel = pixel' then 1 else 0))

let layerWithLeastZeroes =
    layers
    |> Array.map (fun layer -> (layer, pixelsPerLayer 0 layer))
    |> Array.sortBy snd
    |> Array.head
    |> fst

let answer = (pixelsPerLayer 1 layerWithLeastZeroes) * (pixelsPerLayer 2 layerWithLeastZeroes)

// Part 2

let overlayLine (line: Line) (overlay: Line) =
    Array.fold2 (fun (newline:Pixel list) pixel1 pixel2 -> 
                    newline @ [yield if pixel2 = 0 || pixel2 = 1 then pixel2 else pixel1]) [] line overlay
    |> List.toArray

let applyOverlay (layer:Layer) (overlay:Layer) =
    Array.fold2 (fun (newlayer:Line list) line1 line2 -> newlayer @ [yield overlayLine line1 line2]) [] layer overlay
    |> List.toArray

let layers' = Array.rev layers // build up from the last layer to the first layer per directions

let overlayAll =
    layers'
    |> Array.tail 
    |> Array.fold applyOverlay (Array.head layers')

for line in overlayAll do
    for pixel in line do printf "%d" pixel
    printfn ""
