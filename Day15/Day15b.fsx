open System

type ElvesWin = { Rounds: int; SumHP: int; Result:int }
type SimResult = ElFDied | AllElvesAlive of ElvesWin
type UnitType = | Elf | Goblin

type Unit =
  { Id: int
    HP: int
    Attack : int
    Type: UnitType
    Location: int * int}

type TileType =  | Wall | Open

type GameState =
  { Map: TileType[,] }

let isUnoccupied loc units =
  not (Map.exists (fun i u -> u.Location = loc) units)

let isOpenTile (map:TileType[,]) (i, j) units =
  map.[i,j] = Open && isUnoccupied (i, j) units

let inRangeTiles u map units =
  let enemyLocations =
    units
    |> Map.filter (fun id u' -> u'.Type <> u.Type)
    |> Map.toList
    |> List.map (fun (id, u) -> u.Location)

  // printfn "enemy locations %A" enemyLocations
  //all tiles adjacent to a unit that are open and unoccupied

  seq { for (i, j) in enemyLocations do
          //up
          if i > 0 && isOpenTile map (i - 1, j) units then
            yield (i - 1, j)
          //down
          if i < Array2D.length1 map - 1 && isOpenTile map (i + 1, j) units then
            yield (i + 1, j)
          //left
          if j > 0 && isOpenTile map (i, j - 1) units then
            yield (i, j - 1)
          //right
          if j < Array2D.length2 map - 1  && isOpenTile map (i, j + 1) units then
            yield (i, j + 1) }
  |> Seq.distinct




let genMap (input: string list) =
  let height = Seq.length input
  let width = Seq.length (input.[0])
  Array2D.init height width (fun i j ->
    match input.[i].[j] with
    | '#' -> Wall
    | _ -> Open)

let genUnits (input: string list) elfAttack =
  let height = Seq.length input
  let width = Seq.length (input.[0])
  seq { for i in 0..(height-1) do
          for j in 0..(width-1) do
            match input.[i].[j] with
            | 'E' -> yield (Elf, (i, j))
            | 'G' -> yield (Goblin, (i, j))
            | _ -> () }
  |> Seq.mapi (fun i (u, l) ->
    (i, { Id = i
          HP = 200
          Attack = if u = Goblin then 3 else elfAttack
          Location = l
          Type = u }))
  |> Map.ofSeq

let rec explore current map frontier explored units =
  let (i,j), dist = current

  //up
  let frontier =
    if i > 0 && isOpenTile map (i - 1,j) units && not (Map.containsKey (i - 1, j) explored) then
      Set.add ((i - 1, j), dist + 1) frontier
    else
      frontier

  //left
  let frontier =
    if j > 0 && isOpenTile map (i, j - 1) units && (not (Map.containsKey (i, j - 1) explored)) then
      Set.add ((i, j - 1), dist + 1) frontier
    else
      frontier

  //right
  let frontier =
    if j < (Array2D.length2 map - 1) && isOpenTile map (i, j + 1) units && not (Map.containsKey (i, j + 1) explored) then
      Set.add ((i, j + 1), dist + 1) frontier
    else
      frontier

  //down
  let frontier =
    if i < (Array2D.length1 map - 1) && isOpenTile map (i + 1, j) units && not (Map.containsKey (i + 1, j) explored) then
      Set.add ((i + 1, j), dist + 1) frontier
    else
      frontier

  let explored = Map.add (i,j) dist explored
  if frontier = Set.empty then
    explored
  else
    let next = Set.toList frontier |> List.sortBy (fun (_, dist) -> dist) |> List.head
    let frontier = Set.remove next frontier
    explore next map frontier explored units

let distForTile (map:TileType[,]) (i, j) units =
  let explored = Map.empty
  let frontier = Set.empty
  explore ((i, j), 0) map frontier explored units

let getUnitAt loc units =
  Map.tryPick (fun _ u -> if u.Location = loc then Some u else None) units

let printDistGrid (map:TileType[,]) distGrid units =
  let greekLowers = ['α'; 'β'; 'γ'; 'δ'; 'ε'; 'ζ'; 'η'; 'θ'; 'ι'; 'κ'; 'λ'; 'μ'; 'ν'; 'ξ'; 'ο'; 'π'; 'ρ'; 'σ'; 'τ'; 'υ'; 'φ'; 'χ'; 'ψ'; 'ω']
  let lowers = ['a' .. 'z'] @ greekLowers
  for i in 0..Array2D.length1 map - 1 do
    for j in 0..Array2D.length2 map - 1 do
      let unitOpt = getUnitAt (i, j) units
      if Option.isSome unitOpt then
        match unitOpt with
        | Some { Type = Goblin } -> printf "G"
        | Some { Type = Elf } -> printf "E"
        | _ -> failwith "should not have made it here"
      elif isOpenTile map (i,j) units then
        let distOpt = Map.tryFind (i, j) distGrid
        match distOpt with
        | Some dist when dist < 10 ->
          printf "%i" dist
        | Some dist ->
          printf "%c" lowers.[dist]
        | None -> printf "!"
      else
        printf "#"
    printfn ""

let printLocations (map:TileType[,]) units =
  for i in 0..Array2D.length1 map - 1 do
    for j in 0..Array2D.length2 map - 1 do
      let unitOpt = getUnitAt (i, j) units
      if Option.isSome unitOpt then
        match unitOpt with
        | Some { Type = Goblin } -> printf "G"
        | Some { Type = Elf } -> printf "E"
        | _ -> failwith "should not have made it here"
      elif isOpenTile map (i,j) units then
        printf "."
      else
        printf "#"
    printfn ""


let chooseNextMove toDistGrid fromLocation closestDistance =
  //find the locations around the fromLocation
  let (i, j) = fromLocation
  let paths =
    Map.filter (fun l d ->
    ( l = (i - 1, j) ||
      l = (i + 1, j) ||
      l = (i, j - 1) ||
      l = (i, j + 1))) toDistGrid
  |> Map.toList

  let shortestPathLength =
    paths
    |> List.map (fun (_, d) -> d)
    |> List.min

  paths
  |> List.filter (fun (_, d) -> d = shortestPathLength)
  |> List.map (fun (l, _)  -> l)
  |> List.min

let neighbourTilesUnsafe (i,j) =
  [ (i - 1, j)
    (i + 1, j)
    (i, j - 1)
    (i, j + 1)]

let hasEnemyNeighbour (u:Unit) units =
  let neighbourTiles = neighbourTilesUnsafe u.Location
  Map.exists (fun id u' ->
    (u'.Type <> u.Type) &&
    (List.exists (fun n -> n = u'.Location) neighbourTiles)) units

let enemyNeighbours u units =
  let neighbourTiles = neighbourTilesUnsafe u.Location
  Map.filter (fun id u' ->
    (u'.Type <> u.Type) &&
    (List.exists (fun n -> n = u'.Location) neighbourTiles)) units
  |> Map.toList
  |> List.map (fun (_, en) -> en)

let attack u units  =
  if not (hasEnemyNeighbour u units) then
    units
  else
    let target =
      enemyNeighbours u units
      |> List.minBy (fun en -> (en.HP, en.Location))

    let target = { target with HP = target.HP - u.Attack }
    if target.HP < 0 then
      Map.remove target.Id units
    else
      Map.add target.Id target units


let moveUnit (u:Unit) (units:Map<int, Unit>) map =
  // printfn "ticking unit %i %A" u.Id u.Type
  if hasEnemyNeighbour u units then
    u
  else
    let inRangeTiles = inRangeTiles u map units
    // printfn "in range tiles %A" inRangeTiles
    let distGrid = distForTile map u.Location units
    // printDistGrid map distGrid units
    let reachable =
      inRangeTiles
      |> Seq.filter (fun t -> distGrid.ContainsKey t)
      |> Seq.map(fun t -> (t, Map.find t distGrid))

    // printfn "reachable %A" reachable
    if Seq.isEmpty reachable then
      u //no squares are reachable; skip turn
    else
      let closestDistance =
        reachable
        |> Seq.map (fun (_, d) -> d)
        |> Seq.min

      // printfn "closest dist %A" closestDistance
      let targetLocation =
        reachable
        |> Seq.filter (fun (t, d) -> d = closestDistance)
        |> Seq.map (fun (t, _) -> t)
        |> Seq.sort
        |> Seq.head

      let targetDisGrid = distForTile map targetLocation units
      let nextMove = chooseNextMove targetDisGrid u.Location closestDistance
      { u with Location = nextMove }

let printUnits units =
  units
  |> Map.iter (fun _ u ->
    printfn "%A%A hp:%A" u.Type u.Id u.HP)


let rec tick units map startingElfCount iteration =
  let (newState, map) =
    Seq.sortBy (fun u -> u.Location) (units |> Map.toList |> List.map (fun (id, u) -> u))
    |> Seq.map (fun u -> u.Id)
    |> Seq.fold (fun (unitsState, map) uid ->
      let uOpt = Map.tryFind uid unitsState
      match uOpt with
      | None -> (unitsState, map)
      | Some u ->
        // printfn "%A%A Go!" u.Type u.Id
        let u = moveUnit u unitsState map
        let unitsState = Map.add u.Id u unitsState
        let unitsState = attack u unitsState
        (unitsState, map)) (units, map)
  //group count
  let groupCount =
    newState
    |> Map.toList
    |> List.groupBy (fun (_, u) -> u.Type)
    |> List.length

  let sum =
    newState
    |> Map.toList
    |> List.sumBy (fun (_, u) -> u.HP)

  let elfCount =
    newState
    |> Map.filter (fun _ u -> u.Type = Elf) |> Map.count

  if elfCount < startingElfCount then
    printfn "An elf died--stopping simulation after round %A. Elf count %A" iteration elfCount
    ElFDied
  elif groupCount = 1 then
    printfn "sim over after round %A with sum of hp = %A and solution =  %A" iteration sum (iteration * sum)
    AllElvesAlive { Rounds = iteration; SumHP = sum;  Result = (iteration * sum) }
  else
    tick newState map startingElfCount (iteration + 1)


let lines =
  // System.IO.File.ReadLines("C:\Users\john\code\Aoc2018\Day15\inputSample2.txt")
  System.IO.File.ReadLines("C:\Users\john\code\Aoc2018\Day15\input.txt")
  |> List.ofSeq

let map = genMap lines

let rec go elfAttack lines map runsLeft =
  let units = genUnits lines elfAttack
  let startingElfCount = (Map.filter (fun _ u -> u.Type = Elf) units) |> Map.count
  // printfn "starting elf count %A" startingElfCount
  let simResult = tick units map startingElfCount 0
  match simResult, runsLeft with
  | _, 0 ->
    printfn "ran out of attempts killing the simulation"
  | ElFDied, _ ->
    let newElfAttack = elfAttack + 1
    printfn "starting new sim with elf attack = %A" newElfAttack
    go newElfAttack lines map (runsLeft - 1)
  | AllElvesAlive result, _ ->
    printfn "elves win with no losses! %A" result

go 3 lines map 1500


