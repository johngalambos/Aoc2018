let getHun num =
  if abs(num) < 100 then
    0
  else
    (num / 100 % 10)


let gv sNum (x, y) =
  let rackId = x + 10
  let powerLevel = rackId * y
  let powerLevel = sNum + powerLevel
  let powerLevel = powerLevel * rackId
  let powerLevel = getHun powerLevel
  powerLevel - 5

// Fuel cell at  122,79, grid serial number 57: power level -5.
// Fuel cell at 217,196, grid serial number 39: power level  0.
// Fuel cell at 101,153, grid serial number 71: power level  4.
// let ex1 = gv 57 (122, 79)
// let ex2 = gv 39 (217, 196)
// let ex3 = gv 71 (101, 153)

let genLine sNum y =
  [|for x in 1..300 -> gv sNum (x, y)|]

let genGrid sNum =
  [|for y in 1..300 -> genLine sNum y|]

let grid = genGrid 1718

let sumAreaPower (grid: int [] [])(x, y) =
  seq { for y' in 0..2 do
          for x' in 0..2 do
            yield grid.[y - 1 + y'].[x - 1 + x'] }
  |> Seq.sum

let getAreaPower grid =
  seq { for y in 1..298 do
          for x in 1..298 do
            yield ((x,y), sumAreaPower grid (x, y)) }

// let area = getAreaPower grid
// // part 1
// let maxPower = area |> Seq.maxBy (fun ((x, y), power) -> power)

//part 2
let sumAreaPower2 (grid: int [] []) s (x, y) =
  seq { for y' in 0..(s - 1) do
          for x' in 0..(s - 1) do
            yield grid.[y - 1 + y'].[x - 1 + x'] }
  |> Seq.sum

let getMax grid s =
  printfn "working on %A" s
  let coords = seq {  for y in 1..(300 - s + 1) do
                        for x in 1..(300 - s + 1) do
                          yield (x, y) }

  coords
  |> Seq.fold (fun acc (x, y) ->
    let (_, _, prevMax) = acc
    let sum = sumAreaPower2 grid s (x, y)
    if sum > prevMax then
      (x, y, sum)
    else
      acc) (0, 0, 0)

let result2 =
  [|1..300|]
  |> Array.Parallel.map (fun s -> (s, getMax grid s))
  |> Array.maxBy (fun (s, (x,y,power)) -> power)
