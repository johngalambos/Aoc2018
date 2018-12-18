open System
open System.Text.RegularExpressions

type Tile = | Sand | Clay | Water | Spring
type FloorCoords = { y: int; fromX: int; toX: int }
type WallCoords = {x: int; fromY:int; toY: int }
type Coords = Floor of FloorCoords | Wall of WallCoords

type FrontierItem =
  { node: int*int
    stepCount: int
    fork: int }

type Frontier =
  { contents: Map<int, FrontierItem list> }

  member f.Next =
    if Map.isEmpty f.contents then
      None
    else
      let (fork, forkFrontier) =
        f.contents
        |> Map.toList
        |> List.minBy (fun (fork, forkFrontier) ->
          let head = forkFrontier |> List.head
          head.stepCount)

      // printfn  "frontier contains the following forks %A" (f.contents |> Map.toList |> List.map (fun (k, v) -> k))

      match (fork, forkFrontier) with
      | fork, h::[] ->
        Some (h, { f with contents = Map.remove fork f.contents })
      | fork, h::t ->
        Some (h, { f with contents = Map.add fork t f.contents})
      | _ -> failwith "should not hav gotten here"

  member f.RemoveFork fork =
    { f with contents = Map.remove fork f.contents }

  member f.Add node fork stepCount =
    match Map.containsKey fork f.contents with
    | true ->
      // printfn "adding to fork"
      let newList = { node = node; stepCount = stepCount; fork=fork }::(Map.find fork f.contents)
      { f with contents = Map.add fork newList f.contents }
    | false ->
      // printfn "creating a new fork for %A" fork
      { f with contents = Map.add fork [{ node = node; stepCount = stepCount; fork=fork}] f.contents }

  static member Init rootNode fork stepCount=
    let f = { contents = Map.empty }
    f.Add rootNode fork stepCount

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
  printfn "0123456789ABCDEF"
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

let CanExploreTwoRightFrom (i, j) (map: Tile[,]) (explored: Set<int*int>) =
  // printfn "tworight: j %A length %A" j (Array2D.length2 map)
  if j + 2 >= Array2D.length2 map - 1  then
    false
  else
    match map.[i, j + 1], explored.Contains(i,j + 1), map.[i, j + 2], explored.Contains(i,j + 2) with
    | Clay, _, _, _ -> false
    | _, _, Sand, false -> true
    | _ -> false

let CanExplore (i, j) (map: Tile[,]) (explored: Set<int*int>) =
  if i > Array2D.length1 map - 1 || j > Array2D.length2 map - 1 || i < 0 || j < 0 then
    false
  else
    match map.[i, j], explored.Contains(i,j) with
    | Sand, false -> true
    | _ -> false

let rec explore map explored (frontier:Frontier) stepNumber =
  // printfn "frontier: %A" frontier
  match frontier.Next with
  | None -> explored
  | Some ({ node=(i, j); fork=forkNumber}, frontier) ->
    // printfn "exploring %A on fork %A step #%A" (i, j) forkNumber stepNumber
    if i = Array2D.length1 map - 1 then
      let explored = Set.add (i,j) explored
      //remove the whole fork once we hit the bottom
      let frontier = frontier.RemoveFork forkNumber
      explore map explored frontier (stepNumber + 1)
    else
      let explored = Set.add (i,j) explored

      let right = (i, j + 1)
      let left = (i, j - 1)
      let down = (i + 1, j)
      let twoRight = (i, j + 2)

      let canGoRight = CanExplore right map explored
      let canGoLeft = CanExplore left map explored
      let canGoDown = CanExplore down map explored
      let canGoTwoRight = CanExploreTwoRightFrom (i, j) map explored

      //if we can't go down but can go either left or right, this is a fork
      //give the left direction the same fork number as the parent but add a
      //new one for the right direction
      //frontier should be a map of lists by fork which we can update
      //we should choose the list by the lowest
      match canGoRight, canGoTwoRight, canGoLeft, canGoDown with
      // | true, false, true, false ->
      //   // printfn "Creating a new fork"
      //   let frontier = frontier.Add right (forkNumber + 1) stepNumber
      //   let frontier = frontier.Add left (forkNumber) stepNumber
      //   explore map explored frontier (stepNumber + 1)
      | false, true, true, false ->
        // printfn "Creating a new overflow fork"
        let frontier = frontier.Add twoRight (forkNumber + 1) stepNumber
        let frontier = frontier.Add left (forkNumber) stepNumber
        explore map explored frontier (stepNumber + 1)
      | _  ->
        let frontier = if canGoRight then frontier.Add right (forkNumber) stepNumber else frontier
        let frontier = if canGoLeft then frontier.Add left (forkNumber) stepNumber else frontier
        let frontier = if canGoDown then frontier.Add down (forkNumber) stepNumber else frontier
        explore map explored frontier (stepNumber + 1)


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
  // let (map, minX) = genMap testInput
  let input = System.IO.File.ReadLines("C:\Users\john\code\Aoc2018\Day17\input.txt")
  let (map, minX) = genMap input

  let frontier =  Frontier.Init (0, 500 - minX) 0 0
  let explored = explore map Set.empty frontier  0
  // printMapToFile map explored "C:\Users\john\code\Aoc2018\Day17\outputsample.txt"
  printMapToFile map explored "C:\Users\john\code\Aoc2018\Day17\output.txt"

  // printMap map explored
  printfn "explored tiles %A" explored.Count

solve ()

