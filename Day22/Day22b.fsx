open System.Collections.Generic
open System.Numerics
type RegionType = Rocky | Wet | Narrow
type Region =
  { location: int*int
    regionType: RegionType
    erosionLevel: BigInteger
    geologicIndex: BigInteger }


let rec createInitializer depth target =
  let regions = new Dictionary<int*int, Region>()

  let rec describeRegion (i, j) =
    // printfn "describing region %A" (i,j)
    if regions.ContainsKey(i,j) then
      regions.[(i,j)]
    else
      let geoIndex = calcGeologicIndex (i,j) target
      let erosionLevel = calcErosionlevel geoIndex
      let regionType = calcType erosionLevel
      let region =
        { location = (i, j)
          erosionLevel = erosionLevel
          regionType = regionType
          geologicIndex = geoIndex }
      regions.Add(region.location, region)
      region

  and calcGeologicIndex  (i, j) (it, jt) =
    // printfn "target %A" (it, jt)
    match (i,j) with
    | (0,0) -> bigint 0
    | (0,j) -> bigint (j * 16807)
    | (i,0) -> bigint (i * 48271)
    | (i, j) when (i, j) = (it, jt) -> bigint 0
    | (i, j) ->
      let left = describeRegion (i, j - 1)
      let right = describeRegion (i - 1, j)
      left.erosionLevel * right.erosionLevel

  and calcErosionlevel (geologicIndex: bigint) =
    (geologicIndex + depth) % (bigint 20183)

  and calcType (erosionLevel: bigint) =
    match (erosionLevel % (bigint 3)) |> int with
    | 0 -> Rocky
    | 1 -> Wet
    | 2 -> Narrow
    | n -> failwith  (sprintf "unexpected erosion level %i" n)

  describeRegion

let genMap describeRegion (it, jt) =
  Array2D.init (it + 1) (jt + 1) (fun i j ->  describeRegion (i, j))

let printMap map =
  for i in 0..Array2D.length1 map - 1 do
    for j in 0..Array2D.length2 map - 1 do
       match map.[i,j].regionType with
        | Rocky -> printf "."
        | Wet -> printf "="
        | Narrow -> printf "|"
    printfn ""

type Tool = Neither | Torch | Harness

type Action = EquipTorch | EquipHarness | EquipNeither | Left | Right | Up | Down

type State =
  { location: int*int
    equipped: Tool }

type Node =
  { location:  int*int
    action: Action
    pathCost: int
    estPathCost: int
    state: State }


let pop (frontier:Dictionary<State, Node>) =
  let next =
    frontier
    |> Seq.sortBy (fun (kvp) -> kvp.Value.estPathCost)
    |> Seq.head
  frontier.Remove(next.Key) |> ignore
  next.Value

let estimatePathCost equipped (i, j) (it, jt) =
  let dist = abs (it - i)  + abs (jt - j)
  let switch =
    match equipped with
    | Torch -> 0
    | _ -> 7
  dist + switch


let actionToEquipped action =
  match action with
  | EquipTorch -> Torch
  | EquipHarness -> Harness
  | EquipNeither -> Neither
  | _ -> failwith "invalid action"

let (|Equip|Move|) action =
  if List.contains  action [EquipTorch; EquipHarness; EquipNeither] then
    let tool = actionToEquipped action
    Equip (tool)
  else
    Move (action)

let getAdjacent (i, j) dir =
  match dir with
  | Left -> (i, j - 1)
  | Right -> (i, j + 1)
  | Up -> (i - 1, j)
  | Down -> (i + 1, j)
  | _ -> failwith "invalid direction"

let isValidTool regionType tool =
  match regionType, tool with
  | Rocky, (Harness | Torch) -> true
  | Wet, (Harness | Neither) -> true
  | Narrow, (Torch | Neither) -> true
  | _ -> false

let isValidMove = function
  | Left, (_, 0) -> false
  | Up, (0, _) -> false
  | _ -> true

let expand node target describeRegion actions =
  actions
  |> Seq.map (fun a ->
    match a with
    | Equip tool when node.state.equipped = tool -> None
    | Equip tool ->
        let region = node.location |> describeRegion
        if isValidTool region.regionType tool then
        // printfn "tool %A was not equipped %A" tool node.state.equipped
          Some { node with
                    action = a
                    state = { node.state with equipped = tool }
                    pathCost = node.pathCost + 7
                    estPathCost = node.pathCost + 7 + estimatePathCost tool node.location target }
        else None
    | Move dir when (isValidMove (dir, node.location)) ->
        // printfn "trying to move %A" dir
        let dest = getAdjacent node.location dir
        let region = dest |> describeRegion
        if isValidTool region.regionType node.state.equipped then
          Some { node with
                    location = dest
                    action = dir
                    pathCost = node.pathCost + 1
                    estPathCost = node.pathCost + 1 + estimatePathCost node.state.equipped dest target
                    state = { node.state with location = dest } }
        else
          None
    | Move _  -> None
    | x ->  failwith (sprintf "invalid combo %A" x))

let aStarGraphSeach (frontier:Dictionary<State,Node>) (explored: State Set) target actions describeRegion =
  let rec go frontier explored =
    let next = pop frontier
    if (fst next.location) % 100 = 0 then
      printfn "exploring %A after %A with %A at pathcost %i and estPathCost %i" next.location next.action next.state.equipped next.pathCost  next.estPathCost
    else
      ()
    if next.location = target
      && next.state.equipped = Torch
      && not (frontier |>  Seq.exists (fun kvp -> kvp.Value.estPathCost < next.pathCost))
      then
        next.pathCost
    else
      let expanded = expand next target describeRegion actions |> Seq.choose id
      // printfn "Expanded %A" expanded
      let frontier =
        expanded
        |> Seq.fold (fun (frontier:Dictionary<State,Node>) child ->
          if not(Set.contains child.state explored  || frontier.ContainsKey(child.state)) then
            // printfn "adding new node to frontier %A" child
            frontier.Add(child.state, child)
            frontier
          elif (frontier.ContainsKey(child.state) && child.estPathCost < frontier.[child.state].estPathCost) then
            // printfn "replacing frontier node with better option %A" child
            frontier.Remove(child.state) |> ignore
            frontier.Add(child.state, child)
            frontier
          else
            // printfn "node exists in frontier with better options %A" child
            frontier) frontier
      // printfn "new frontier %A " frontier
      // printfn "explored %A" explored
      go frontier (Set.add next.state explored)
  go frontier explored

// let target = (10, 10)
// let depth = bigint 510
let target = (757, 12)
let depth = bigint 3198

let origin = {
  location = (0, 0);
  pathCost = 0;
  estPathCost =  estimatePathCost Torch (0, 0) target
  action = EquipTorch;
  state = { equipped = Torch; location = (0,0) } }

// printfn "origin %A" origin
let frontier = new Dictionary<State, Node>()
frontier.Add(origin.state, origin)
let explored = Set.empty
let actions = [ EquipTorch; EquipHarness; EquipNeither; Left; Right; Up; Down ]
let describeRegion = createInitializer depth target
let result = aStarGraphSeach frontier explored target actions describeRegion

// let map = genMap describeRegion target


// printMap map
// let shortestRoute map target origin =

