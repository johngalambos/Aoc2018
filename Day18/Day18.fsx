type AreaType = | Open | LumberYard | Trees

let sampleInput =
  [ ".#.#...|#."
    ".....#|##|"
    ".|..|...#."
    "..|#.....#"
    "#.#|||#|#|"
    "...#.||..."
    ".|....|..."
    "||...#|.#|"
    "|.||||..|."
    "...#.|..|." ]

let advanceTile (i, j) (currentState: AreaType[,]) =
  let oldType = currentState.[i,j]
  let top = i - 1
  let bottom = i + 1
  let left = j - 1
  let right = j + 1
  let neighbours =
    seq { for i' in top..bottom do
            for j' in left..right do
              match i',j' with
              | -1, _ -> ()
              | _, -1 -> ()
              | loc when loc = (i, j)  -> ()
              | i', _ when i' >= Array2D.length1 currentState  -> ()
              | _, j'_ when j' >= Array2D.length2 currentState  -> ()
              | i', j' -> yield currentState.[i',j'] }

  let groupedNeighours = Seq.groupBy (fun at -> at) neighbours |> Map.ofSeq

  match oldType with
  | Open ->
    if groupedNeighours.ContainsKey Trees && Seq.length (groupedNeighours.[Trees]) >= 3 then
      Trees
    else
      Open
  | Trees ->
    if groupedNeighours.ContainsKey LumberYard && Seq.length (groupedNeighours.[LumberYard]) >= 3 then
      LumberYard
    else
      Trees
  | LumberYard ->
    if (groupedNeighours.ContainsKey LumberYard && Seq.length (groupedNeighours.[LumberYard]) >= 1)
      && (groupedNeighours.ContainsKey Trees && Seq.length (groupedNeighours.[Trees]) >= 1)  then
      LumberYard
    else
      Open

let printMap (map: AreaType[,]) =
  for i in 0..Array2D.length1 map - 1 do
    for j in 0..Array2D.length2 map - 1 do
      match map.[i,j] with
      | Open -> printf "."
      | LumberYard -> printf "#"
      | Trees -> printf "|"

let groupByType (state: AreaType[,]) =
  seq { for i in 0..Array2D.length1 state - 1 do
          for j in 0..Array2D.length2 state - 1 do
            yield state.[i, j] }
  |> Seq.groupBy id

let tick (state: AreaType[,]) tickNum=
  //create a new 2D array for the output
  let newState = Array2D.init (Array2D.length1 state) (Array2D.length2 state) (fun i j -> Open)
  for i in 0..Array2D.length1 state - 1 do
    for j in 0..Array2D.length2 state - 1 do
      let newTile = advanceTile (i, j) state
      newState.[i,j] <- newTile
      // printfn "working with %A %A i j" i j

  let groups = groupByType newState |> Map.ofSeq
  let trees = Seq.length groups.[Trees]
  let ly = Seq.length groups.[LumberYard]
  let solution = trees * ly
  printfn "tick Num %A  trees: %A ly: %A soln %A" tickNum trees ly solution
  // printMap newState
  newState


let getMap input =
  Array2D.init (List.length input) (String.length input.[0]) (fun i j ->
    match input.[i].[j] with
    | '.' -> Open
    | '|' -> Trees
    | '#' -> LumberYard
    | _ -> failwith "unexpected input")


let input = System.IO.File.ReadAllLines("C:\Users\john\code\Aoc2018\Day18\input.txt") |> Array.toList
// let map = getMap sampleInput
let map = getMap input

printfn "initial state"
// printMap map

let result =
  { 0..1000000000 }
  |> Seq.fold tick map

// let groupByType result  =
//   seq { for i in 0..Array2D.length1 result - 1 do
//           for j in 0..Array2D.length2 result - 1 do
//             yield result.[i, j] }
//   |> Seq.groupBy id

// let groups = groupByType result |> Map.ofSeq

// let trees = Seq.length groups.[Trees]
// let ly = Seq.length groups.[LumberYard]

// let solution = trees * ly
let r1 = (1000000000L  - 3996L) / 28L
let r2 = 3996L + (28L) * r1


// printMap result


