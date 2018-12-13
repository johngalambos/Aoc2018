// Learn more about F# at http://fsharp.org

open System

type Turn = | Left | Right | Straight

type Direction = | Left | Right | Up | Down

type GridTile =
    Vertical
  | Horizontal
  | UpRightLeftDown
  | UpLeftRightDown
  | Intersection
  | Empty

type VehicleState =
  { CurrentDirection: Direction
    Location: int*int
    LastTurn: Turn }


let input =
  // System.IO.File.ReadLines(@"/Users/john/code/Aoc2018/Day13/inputSample.txt")
  System.IO.File.ReadLines(@"/Users/john/code/Aoc2018/Day13/input.txt")
  |> Seq.toArray

let height = Seq.length input

let width =
  input
  |> Seq.maxBy (fun l -> l.Length)
  |> Seq.length

let grid = Array2D.init height width (fun i j ->
  if j >= input.[i].Length then
    Empty
  else
    match input.[i].[j] with
    | ' ' -> Empty
    | '/' -> UpRightLeftDown
    | '\\' -> UpLeftRightDown
    | '-' | '<' | '>' -> Horizontal
    | '|' | '^' | 'v' -> Vertical
    | '+' -> Intersection
    | s -> failwith ("unexpected input:" + s.ToString()))


let startingStates =
  seq { for i in 0..height - 1 do
          for j in 0..width - 1 do
            if j >= input.[i].Length then
              ()
            else
              let c = input.[i].[j]
              match c with
              | 'v' -> yield { CurrentDirection = Down; Location = (j, i); LastTurn = Turn.Right }
              | '^' -> yield { CurrentDirection = Up; Location = (j, i); LastTurn = Turn.Right }
              | '<' -> yield { CurrentDirection = Left; Location = (j, i); LastTurn = Turn.Right }
              | '>' -> yield { CurrentDirection = Right; Location = (j, i); LastTurn = Turn.Right }
              | _ -> () }
  |> Seq.toList

let nextTurn (lastTurn: Turn) : Turn =
  match lastTurn with
  | Turn.Right -> Turn.Left
  | Turn.Left -> Turn.Straight
  | Turn.Straight -> Turn.Right

let dirFromTurn (dir:Direction) (turn:Turn) : Direction =
  match dir, turn with
  | Direction.Left, Turn.Left -> Direction.Down
  | Direction.Left, Turn.Right -> Direction.Up
  | Direction.Right, Turn.Left -> Direction.Up
  | Direction.Right, Turn.Right -> Direction.Down
  | Direction.Down, Turn.Left -> Direction.Right
  | Direction.Down, Turn.Right -> Direction.Left
  | Direction.Up, Turn.Left -> Direction.Left
  | Direction.Up, Turn.Right -> Direction.Right
  | d, Turn.Straight -> d

let dirFromTile dir tile =
  match dir, tile with
  | Left, UpRightLeftDown -> Down
  | Left, UpLeftRightDown -> Up
  | Right, UpLeftRightDown -> Down
  | Right, UpRightLeftDown -> Up
  | Up, UpRightLeftDown -> Right
  | Up, UpLeftRightDown -> Left
  | Down, UpLeftRightDown -> Right
  | Down, UpRightLeftDown -> Left
  | h, Horizontal when (h = Left || h = Right) -> h
  | v, Vertical when (v = Up || v = Down) -> v
  | _ -> failwith "unexpected combo"


let tickVehicle vs (grid: GridTile [,]) =
  let (x, y) = vs.Location

  let (x', y') =
    match vs.CurrentDirection with
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)

  let hasTurn =
    if grid.[y', x'] = Intersection then true else false

  let newDirection =
    match grid.[y',x'] with
    | Intersection ->
      nextTurn vs.LastTurn
      |> dirFromTurn vs.CurrentDirection
    | g -> dirFromTile vs.CurrentDirection g

  { CurrentDirection = newDirection
    Location = (x', y')
    LastTurn = if hasTurn then nextTurn vs.LastTurn else vs.LastTurn }



//tick until we get a collision
let rec tick (states: VehicleState list) (grid: GridTile [,]) tickNum =
  let vehicles =
    states
    |> List.sortBy (fun s ->
      let (x, y) = s.Location
      (y, x))

  let newStates =
    vehicles
    |> List.map (fun v -> tickVehicle v grid)

  printfn "tick num %A" tickNum
  newStates
  |> Seq.iter (fun s -> printfn "%A" s)

  let groupByLocation =
    newStates
    |> Seq.groupBy (fun s -> s.Location)

  let hasCollision =
    groupByLocation
    |> Seq.exists (fun (g, items) -> (Seq.length items) > 1)

  match hasCollision with
  | true -> groupByLocation |> Seq.filter (fun (g, items) -> (Seq.length items) > 1)
  | false -> tick newStates grid (tickNum + 1)


[<EntryPoint>]
let main argv =
  let result = tick startingStates grid 1
  printfn "%A" result
  0 // return an integer exit code
