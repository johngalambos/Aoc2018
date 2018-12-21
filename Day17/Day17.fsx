open System
open System.Text.RegularExpressions
open System.Collections.Generic

type FloorCoords = { y: int; fromX: int; toX: int }
type WallCoords = {x: int; fromY:int; toY: int }
type Coords = Floor of FloorCoords | Wall of WallCoords
let wallRegex = new Regex("^x=(\d+),\s+y=(\d+)..(\d+)$")
let floorRegex = new Regex("^y=(\d+),\s+x=(\d+)..(\d+)$")

type Tile = Sand | Clay | Flowing | Pooled | Falling | Spring | Bounds

type Action = Down | Left | Right

type Node =
  { location: (int * int)
    parent: Node option
    action: Action
    fork: int
    mutable state: Tile }

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
      | Flowing -> printf "-"
      | Falling -> printf "|"
      | Pooled -> printf "~"
      | Spring -> printf "+"
      | Bounds -> printf "*"

    printfn "  %A" i

let printMapToFile (map:Tile[,]) path =
  let lines =
    seq { for i in 0..Array2D.length1 map - 1 do
            let line =
              seq { for j in 0..Array2D.length2 map - 1 do
                      yield
                        match map.[i,j] with
                        | Clay -> sprintf "#"
                        | Sand -> sprintf "."
                        | Flowing -> sprintf "-"
                        | Falling -> sprintf "|"
                        | Pooled -> sprintf "~"
                        | Spring -> sprintf "+"
                        | Bounds -> sprintf "*" }
              |> String.concat ""
            yield line }

  System.IO.File.WriteAllLines(path, lines)

let tileState (i, j) map =
  if i > Array2D.length1 map - 1 || j > Array2D.length2 map - 1 || i < 0 || j < 0 then
    Bounds
  else
    map.[i, j]

let rec boundsLeft (i,j) map  =
  let left = tileState (i, j - 1) map
  let leftDown = tileState (i + 1, j - 1) map
  match (left, leftDown) with
  | Clay, (Clay|Pooled)  -> Some (i, j - 1)
  | _, (Sand|Falling|Flowing) -> None
  | _ -> boundsLeft (i, j - 1) map

let rec boundsRight (i,j)  map =
  let right = tileState (i, j + 1) map
  let rightDown = tileState (i + 1, j + 1) map
  match (right, rightDown) with
  | Clay, (Clay|Pooled)  -> Some (i, j + 1)
  | _, (Sand|Falling|Flowing) -> None
  | _ -> boundsRight (i, j + 1) map


let containedBounds (i,j) map =
  (boundsLeft (i,j) map, boundsRight (i,j) map)
  //

let getNextFork =
  let mutable count = 0
  (fun () -> count <- count + 1; count)

let getNextNodeId =
  let mutable count = 0
  (fun () -> count <- count + 1; count)

let getIteration =
  let mutable count = 0
  (fun () -> count <- count + 1; count)

let nodeLookup = new Dictionary<(int*int), Node>()


let rec flowFrom node (map:Tile[,]) (nodeLookup: Dictionary<int*int, Node>) =
  let iteration = getIteration ()
  if (iteration > 1000000000) then
    ()
  else
    let (i, j) = node.location
    printfn "exploring node (%i, %i)" i j
    map.[i,j] <- node.state
    // printMap map
    let right = (i, j + 1)
    let left = (i, j - 1)
    let down = (i + 1, j)

    match (tileState down map), (tileState left map), (tileState right map), node.action with
    | Bounds, _, _, _ -> ()
    | Sand, _, _, _->
      let downNode = { location = down; action = Down; fork=node.fork; parent = Some node; state=Falling }
      nodeLookup.Add(downNode.location, downNode)
      flowFrom downNode map nodeLookup
    | (Flowing | Falling ), _, _, _ when node.action = Down && node.fork <> nodeLookup.[down].fork->
      printfn "killing fork for whose work is done: %i " node.fork
      ()
    | _, Sand, Sand, _ ->
      let leftNode = { location = left; action=Left; fork=node.fork; parent = Some node; state=Flowing  }
      nodeLookup.Add(leftNode.location, leftNode)
      flowFrom leftNode map nodeLookup
      // if the node is still sand after the left side rruns, run the right side
      if tileState right map = Sand then
        let rightNode = { location = right; action=Right; fork=getNextFork(); parent = Some node; state=Flowing }
        printfn "Executing fork %i starting at %A action %A" rightNode.fork  right rightNode.action
        nodeLookup.Add(rightNode.location, rightNode)
        flowFrom rightNode map nodeLookup
      else
        ()
    | _, Sand, _, _ ->
      let leftNode = { location = left; action=Left; fork=node.fork; parent = Some node; state=Flowing }
      nodeLookup.Add(leftNode.location, leftNode)
      flowFrom leftNode map nodeLookup
    | _, _, Sand, _ ->
      printfn "flowing right"
      let rightNode = { location = right; action=Right; fork=node.fork; parent = Some node; state=Flowing }
      // printfn "creating fork %i"  rightNode.fork
      nodeLookup.Add(rightNode.location, rightNode)
      flowFrom rightNode map nodeLookup
    | _, (Clay|Flowing), _, Left ->
        printfn "rewinding from blocked left (%i, %i) fork:%i" i j node.fork
        //only set it to pooled if it's bound to the  right as well
        // if isBoundedRight (i,j) map then
        //   node.state <- Pooled
        //   map.[i,j] <- Pooled
        match node.parent with
        | Some p when p.fork <> node.fork -> printfn "rewound into a parent fork. dying"
        | Some p -> flowFrom p map nodeLookup
        | None -> ()
    | _, _, (Clay|Flowing), Right ->
        printfn "rewinding from blocked right (%i, %i) fork:%i" i j node.fork
        // if isBoundedLeft (i,j) map then
        //   node.state <- Pooled
        //   map.[i,j] <- Pooled
        match node.parent with
        | Some p when p.fork <> node.fork -> printfn "rewound into a parent fork. dying"
        | Some p -> flowFrom p map nodeLookup
        | None -> ()
    | _ ->
      printfn "rewinding up"
      //don't ever back up to a parent on a different fork
      //confirm pooling when backing UP out of a node

      match node.action, node.parent with
      | _, Some p when p.fork <> node.fork ->
        printfn "this child fork has rewound to parent. Killing it %i" node.fork
        ()
      | Down, Some p when p.fork = node.fork ->
        match containedBounds node.location map with
        | (Some (iLeft, jLeft), Some (iRight, jRight)) ->
          printfn " this level was pooled between bounds %A %A" (iLeft, jLeft) (iRight, jRight)
          for jPooled in (jLeft + 1)..(jRight - 1) do
            let nodeToTag = nodeLookup.[i, jPooled]
            nodeToTag.state <- Pooled
            map.[i, jPooled] <- Pooled
          flowFrom p map nodeLookup
        | _ ->
          printfn "simple rewing for action  %A and parent fork %A"  node.action node.parent.Value.fork
          flowFrom p map nodeLookup
      | _ -> printfn "must have been a parent node on a different fork node action %A (%i,%i)" node.action i j

printfn "starting"
// let testInput = [
//   "x=495, y=2..7"
//   "y=7, x=495..501"
//   "x=501, y=3..7"
//   "x=498, y=2..4"
//   "x=506, y=1..2"
//   "x=498, y=10..13"
//   "x=504, y=10..13"
//   "y=13, x=498..504" ]

let testInput = [
  "x=495, y=3..7"
  "y=7, x=495..501"
  "x=500, y=3..7"
  "x=498, y=3..4"
  "x=506, y=1..2"
  "x=498, y=10..13"
  "x=504, y=10..13"
  "y=13, x=498..504" ]


// let (map, minX) = genMap testInput
// let rootNode = { location = (0, 500 - minX); action = Down; fork=getNextFork(); parent = None; state = Falling }
// nodeLookup.Add(rootNode.location, rootNode)
// flowFrom rootNode map nodeLookup
// printMap map



let input = System.IO.File.ReadLines("C:\Users\john\code\Aoc2018\Day17\input.txt")
let (map, minX) = genMap input
let rootNode = { location = (0, 500 - minX); action = Down; fork=getNextFork(); parent = None; state = Falling }
nodeLookup.Add(rootNode.location, rootNode)
flowFrom rootNode map nodeLookup
printMapToFile map "C:\Users\john\code\Aoc2018\Day17\output.txt"

let result =
  seq { for i in 0..Array2D.length1 map - 1 do
          for j in 0..Array2D.length2 map - 1 do
            yield map.[i,j] }
  |> Seq.filter(fun t ->
    match t with
    | (Flowing|Pooled|Falling) -> true
    | _ -> false)
  |> Seq.length



let result2 =
  seq { for i in 0..Array2D.length1 map - 1 do
          for j in 0..Array2D.length2 map - 1 do
            yield map.[i,j] }
  |> Seq.filter(fun t ->
    match t with
    | (Pooled) -> true
    | _ -> false)
  |> Seq.length

