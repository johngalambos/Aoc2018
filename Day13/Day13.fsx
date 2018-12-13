open System

type VehicleTurn = Left | Right | Straight

type Direction = Left | Right | Up | Down

type GridTile = Vertical | Horizontal | UpRightLeftDown | UpLeftRightDown  | Intersection | Empty

type VehicleState =
  { CurrentDirection: Direction
    Location: int*int
    LastTurn: VehicleTurn }


let input = System.IO.File.ReadLines(@"C:\Users\john\code\Aoc2018\Day13\inputSample.txt") |> Seq.toArray

let height = Seq.length input
let width =
  input
  |> Seq.maxBy (fun l -> l.Length)
  |> Seq.length


let grid = Array2D.init height width (fun i j ->
  printfn "working on i %A j %A" i j
  if input.[i].Length <= j then
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
  seq { for i in 0..height do
          for j in 0..width do
            let c = input.[i].[j]
            match c with
            | 'v' -> yield { CurrentDirection = Down; Location = (j, i); LastTurn = Straight }
            | '^' -> yield { CurrentDirection = Up; Location = (j, i); LastTurn = Straight }
            | '<' -> yield { CurrentDirection = Left; Location = (j, i); LastTurn = Straight }
            | '>' -> yield { CurrentDirection = Right; Location = (j, i); LastTurn = Straight }
            | _ -> () }
  |> Seq.toList

startingStates
|> Seq.iter (fun s -> printfn "state %A" s)



