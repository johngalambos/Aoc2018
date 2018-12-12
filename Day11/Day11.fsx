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

let genGrid2d sNum =
  Array2D.init 300 300  (fun i j -> gv sNum (j + 1, i + 1))

let grid = genGrid2d 1718

let sumAreaPower (grid: int [,])(x, y) =
  seq { for x' in 0..2 do
          for y' in 0..2 do
            yield grid.[y - 1 + y', x - 1 + x'] }
  |> Seq.sum

let getAreaPower grid =
  seq { for y in 1..298 do
          for x in 1..298 do
            yield ((x,y), sumAreaPower grid (x, y)) }

// let area = getAreaPower grid
// // part 1
// let maxPower = area |> Seq.maxBy (fun ((x, y), power) -> power)

//part 2
let sumAreaPower2 (grid: int [,]) s (x, y) =
  seq { for y' in 0..(s - 1) do
          for x' in 0..(s - 1) do
            yield grid.[y - 1 + y', x - 1 + x'] }
  |> Seq.sum

let sumAreaPower2Fast (grid: int [,]) s (x, y) =
  let mutable total = 0

  for y' in 0..(s - 1) do
    for x' in 0..(s - 1) do
      total <- total + grid.[y - 1 + y', x - 1 + x']

  total

let getMax grid s =
  printfn "working on %A" s
  let coords = seq {  for y in 1..(300 - s + 1) do
                        for x in 1..(300 - s + 1) do
                          yield (x, y) }

  coords
  |> Seq.fold (fun acc (x, y) ->
    let (_, _, prevMax) = acc
    let sum = sumAreaPower2Fast grid s (x, y)
    if sum > prevMax then
      (x, y, sum)
    else
      acc) (0, 0, 0)


let getMaxFast grid s =
  // (x, y, sum)
  let mutable currentMax = (0, 0, 0)
  let mutable prevMax = 0
  printfn "working on %A" s

  for y in 1..(300 - s + 1) do
    for x in 1..(300 - s + 1) do
      let sum = sumAreaPower2 grid s (x, y)
      if sum > prevMax then
        currentMax <- (x, y, sum)
        prevMax <- sum
        printfn "new max %A" currentMax
      else
        ()
  currentMax

let result2 =
  [|1..300|]
  |> Array.Parallel.map (fun s -> (s, getMax grid s))
  |> Array.maxBy (fun (s, (x,y,power)) -> power)
