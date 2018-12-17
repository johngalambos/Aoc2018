open System
open System.Text.RegularExpressions

type Tile = | Sand | Clay | Water | Spring
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

let printMap (map:Tile[,]) (explored: Set<int*int>) =
  for i in 0..Array2D.length1 map - 1 do
    for j in 0..Array2D.length2 map - 1 do
      match map.[i,j] with
      | Clay -> printf "#"
      | Sand ->
        if explored.Contains(i,j) then
          printf "e"
        else
          printf "."
      | Spring -> printf "+"
      | _ -> printf "?"

    printfn ""

let Add (i, j) map explored =
  match map.[i, j], explored.Contains(i,j) with
  | Sand, false ->
    right::frontier
  | _ -> frontier

let rec explore map explored frontier rights =
  match frontier, falls with
  | [] -> failwith "expected a node"
  | (i, j)::frontier ->
    // printfn "exploring %A" (i, j)
    if i = Array2D.length1 map - 1 then
      explored
    else
      let explored = Set.add (i,j) explored

      let right = (i, j + 1)
      let left = (i, j - 1)
      let down = (i + 1, j)
      //if underneath this is already explored or is tile then we should go
      //going left and right before going down again

      let frontier = Add right map explored
      let frontier = Add left map explored
      let frontier = Add down map explored
      explore map explored frontier



[<EntryPoint>]
let main argv =
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
    // let input = System.IO.File.ReadLines("/Users/john/code/Aoc2018/Day17/input.txt")
    // let map = genMap input
    let explored = explore map Set.empty [(0, 500 - minX)]
    printMap map explored
    printfn "explored tiles %A" explored.Count
    0 // return an integer exit code
