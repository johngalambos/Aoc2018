open System
open System.Text.RegularExpressions

type Tile = | Sand | Clay | Flowing | Pooled | Spring
type FloorCoords = { y: int; fromX: int; toX: int }
type WallCoords = {x: int; fromY:int; toY: int }
type Coords = Floor of FloorCoords | Wall of WallCoords
let wallRegex = new Regex("^x=(\d+),\s+y=(\d+)..(\d+)$")
let floorRegex = new Regex("^y=(\d+),\s+x=(\d+)..(\d+)$")

let genMap input =
  let coords =
    input
    |> Seq.map(fun l ->
      let wallMatch = wallRegex.Match(l)
      let floorMatch = floorRegex.Match(l)
      match wallMatch.Success, floorMatch.Success with
      | true, false ->
        Wall {
          x = wallMatch.Groups.[1].Value |> int
          fromY = wallMatch.Groups.[2].Value |> int
          toY = wallMatch.Groups.[3].Value |> int }
      | false, true ->
        Floor {
          y = floorMatch.Groups.[1].Value |> int
          fromX = floorMatch.Groups.[2].Value |> int
          toX = floorMatch.Groups.[3].Value |> int }
      | _ -> failwith "unexpected input")

  let minY =
    coords
    |> Seq.map (fun c ->
      match c with
      | Floor { y=y } -> y
      | Wall { fromY=fromY } -> fromY)
    |> Seq.min
  let maxY =
    coords
      |> Seq.map(fun c ->
      match c with
      | Floor { y=y } -> y
      | Wall { toY=toY } -> toY)
    |> Seq.max

  let minX =
    (coords
    |> Seq.map (fun c ->
      match c with
      | Floor { fromX=fromX } -> fromX
      | Wall { x=x } -> x)
    |> Seq.min) - 1

  let maxX =
    (coords
    |> Seq.map (fun c ->
      match c with
      | Floor { toX=toX } -> toX
      | Wall { x=x } -> x)
    |> Seq.max) + 1


  printfn "minY %A maxY %A minX %A maxX %A" minY maxY minX maxX

  let map =
    Array2D.init
      (maxY - minY + 1)
      (maxX - minX + 1)
      (fun i j -> Sand)

  for coord in coords do
    match coord with
    | Wall { x=x; fromY=fromY; toY=toY } ->
      for i in fromY..toY do
        map.[i - minY, x - minX] <- Clay
    | Floor { y=y; fromX=fromX; toX=toX } ->
      for j in fromX..toX do
        map.[y - minY, j - minX] <- Clay
  //set the source
  map.[0, 500-minX] <- Spring
  (map, minX)

let printMap (map:Tile[,]) =
  printfn "0123456789ABCDEF"
  for i in 0..Array2D.length1 map - 1 do
    for j in 0..Array2D.length2 map - 1 do
      match map.[i,j] with
      | Clay -> printf "#"
      | Sand -> printf "."
      | Spring -> printf "+"
      | _ -> printf "?"

    printfn "  %A" i

let printMapToFile (map:Tile[,]) (explored: Set<int*int>) path =
  let lines = seq { for i in 0..Array2D.length1 map - 1 do
                      let line =
                        seq { for j in 0..Array2D.length2 map - 1 do
                                yield match map.[i,j] with
                                      | Clay -> sprintf "#"
                                      | Sand ->
                                        if explored.Contains(i,j) then
                                          sprintf "~"
                                        else
                                          sprintf "."
                                      | Spring -> sprintf "+"
                                      | _ -> sprintf "?" }
                        |> String.concat ""
                      yield line }
  lines
  |> Seq.iter (fun l -> printfn "%s" l)

  System.IO.File.WriteAllLines(path, lines)

let CanFlowTo (i, j) (map: Tile[,]) =
  if i > Array2D.length1 map - 1 || j > Array2D.length2 map - 1 || i < 0 || j < 0 then
    false
  else
    match map.[i, j] with
    | Sand | Flowing -> true
    | _ -> false

let rec flowFrom (i, j) (map: Tile[,]) step =
  printfn "step #%i (%i, %i) " step i j

  map.[i, j] <- Flowing

  match map.[i + 1, j] with
  | Sand | Spring ->
    map.[i + 1, j] <- Flowing
    flowFrom (i + 1, j) map (step + 1)
  | Clay ->
    if CanFlowTo (i, j + 1) map then
      flowFrom (i, j + 1) map (step + 1)
    if CanFlowTo (i, j - 1) map then
      flowFrom (i, j - 1) map (step + 1)
  | t ->
    failwith (sprintf "need to handle this case %A" t)

  ()


let solve () =
  printfn "starting"
  let testInput = [
    "x=495, y=2..7"
    "y=7, x=495..501"
    "x=501, y=3..7"
    "x=498, y=2..4"
    "x=506, y=1..2"
    "x=498, y=10..13"
    "x=504, y=10..13"
    "y=13, x=498..504" ]
  let (map, minX) = genMap testInput
  flowFrom (0, 500 - minX) map 0
  printMap map
  // let input = System.IO.File.ReadLines("C:\Users\john\code\Aoc2018\Day17\input.txt")
  // let (map, minX) = genMap input

  // let frontier =  Frontier.Init (0, 500 - minX) 0 0
  // let explored = explore map Set.empty frontier  0
  // printMapToFile map explored "C:\Users\john\code\Aoc2018\Day17\outputsample.txt"
  // printMapToFile map explored "C:\Users\john\code\Aoc2018\Day17\output.txt"

  // printMap map explored

solve ()

