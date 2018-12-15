open System

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

let isUnoccupied loc  units =
  not (Seq.exists (fun u -> u.Location = loc) units)

let inRangeTiles u gridDist units =
  let enemyLocations =
    units
    |> Seq.filter (fun u' -> u'.Type <> u.Type)
    |> Seq.map (fun u -> u.Location)

  let unOccupiedAdjLoc =
    seq { for el in enemyLocations do
            let adjLoc =
              Map.find el gridDist
              |> Map.filter (fun l d -> d = 1 && isUnoccupied l units)
              |> Map.toList
              |> List.map (fun (l,_) -> l)
            yield! adjLoc }
  unOccupiedAdjLoc


// let tickUnit u =
  ////find the nearest opponent
  ////try to move towards it
  ////attack if possible
//let tick grid locations


let genMap input: string list) =
  let height = Seq.length input
  let width = Seq.length (input.[0])
  Array2D.init height width (fun i j ->
    match input.[i].[j] with
    | '#' -> Wall
    | _ -> Open)

let genUnits (input: string list) =
  let height = Seq.length input
  let width = Seq.length (input.[0])
  seq { for i in 0..(height-1) do
          for j in 0..(width-1) do
            match input.[i].[j] with
            | 'E' -> yield (Elf, (i, j))
            | 'G' -> yield (Goblin, (i, j))
            | _ -> () }
  |> Seq.mapi (fun i (u, l) ->
    { Id = i
      HP = 200
      Attack = 3
      Location = l
      Type = u })

let isOpenTile (map:TileType[,]) (i, j) =
  map.[i,j] = Open


let rec explore current origin map frontier explored =
  let (i,j), dist = current
  let (iO, jO) = origin
  // printfn "exploring %A" current

  //up
  let frontier =
    if i > 0 && isOpenTile map (i - 1,j) && not (Map.containsKey (i - 1, j) explored) then
      // printfn "adding up to the frontier %A" (i - 1, j)
      Set.add ((i - 1, j), dist + 1) frontier
    else
      frontier

  //left
  let frontier =
    if j > 0 && isOpenTile map (i, j - 1) && (not (Map.containsKey (i, j - 1) explored)) then
      // printfn "adding left to the frontier %A " (i, j-1)
      Set.add ((i, j - 1), dist + 1) frontier
    else
      frontier

  //right
  let frontier =
    if j < (Array2D.length2 map - 1) && isOpenTile map (i, j + 1) && not (Map.containsKey (i, j + 1) explored) then
      // printfn "adding right to the frontier %A " (i, j+1)
      Set.add ((i, j + 1), dist + 1) frontier
    else
      frontier

  //down
  let frontier =
    if i < (Array2D.length1 map - 1) && isOpenTile map (i + 1, j) && not (Map.containsKey (i + 1, j) explored) then
      // printfn "adding down to the frontier %A " (i + 1, j)
      Set.add ((i + 1, j), dist + 1) frontier
    else
      frontier

  let explored = Map.add (i,j) dist explored
  if frontier = Set.empty then
    explored
  else
    let next = Set.toList frontier |> List.sortBy (fun (_, dist) -> dist) |> List.head
    let frontier = Set.remove next frontier
    explore next origin map frontier explored

// TileType[,] -> int * int -> Map<int*int, dist>
let distForTile (map:TileType[,]) (i, j) =
  let explored = Map.empty //the distance to all tiles from this tile
  let frontier = Set.empty //tile we need to explore
  explore ((i, j), 0) (i, j) map frontier explored

// genDistGrid :: map -> Map<int*int, Map<int*int, dist>>
let genDistGrid (map:TileType[,]) =
  let openTiles =
    seq { for i in 0..(Array2D.length1 map - 1) do
            for j in 0..(Array2D.length2 map - 1) do
              if map.[i, j] = Open then
                yield (i, j) }

  openTiles
  |> Seq.fold (fun s (i, j) ->
    let dft = distForTile map (i, j)
    Map.add (i, j) dft s) Map.empty



let sample1 =
  [ "#######"
    "#E..G.#"
    "#...#.#"
    "#.G.#G#"
    "#######" ]

let minReadingOrder t1 t2 =
  match t1, t2 with
  | (i1, j1), (i2, j2) when  i1 < i2 -> (i1, j1)
  | (i1, j1), (i2, j2) when  i1 > i2 -> (i2, j2)
  | (i1, j1), (i2, j2) when  i1 = i2 && j1 < j2 -> (i1, j1)
  | (i1, j1), (i2, j2) when  i1 = i2 && j1 > j2 -> (i2, j2)
  | t1, t2 when  t1 = t2 -> t1
  | _ -> failwith "should not have made it here"

let maxReadingOrder t1 t2 =
  let minOrder = minReadingOrder t1 t2
  if minOrder = t1 then t2 else t1

let getInterTileDist t1 t2 distGrid =
  let upperLeft = minReadingOrder t1 t2
  let bottomRight = maxReadingOrder t1 t2
  // printfn "intertile ul %A br %A" upperLeft bottomRight
  Map.find upperLeft distGrid
  |> Map.find  bottomRight

let printDistGrid (map:TileType[,]) distGrid (i',j') =
  let greekLowers = ['α'; 'β'; 'γ'; 'δ'; 'ε'; 'ζ'; 'η'; 'θ'; 'ι'; 'κ'; 'λ'; 'μ'; 'ν'; 'ξ'; 'ο'; 'π'; 'ρ'; 'σ'; 'τ'; 'υ'; 'φ'; 'χ'; 'ψ'; 'ω']
  let lowers = ['a' .. 'z'] @ greekLowers
  for i in 0..Array2D.length1 map - 1 do
    for j in 0..Array2D.length2 map - 1 do
      if isOpenTile map (i,j) then
        let dist = getInterTileDist (i,j) (i',j') distGrid
        if dist < 10 then
          printf "%i" dist
        else
          printf "%c" lowers.[dist]

      else
        printf "#"
    printfn ""

let printUnits units =
  units
  |> Seq.iter (fun u -> printfn "%A" u)

// let map = genMap sample1

let lines =
  System.IO.File.ReadLines("C:\Users\john\code\Aoc2018\Day15\input.txt")
  |> List.ofSeq

let units = genUnits lines
printUnits units
let map = genMap lines

let distGrid = genDistGrid map
printDistGrid map distGrid (30, 17)

let firstUnit = units |> Seq.head
let irt = inRangeTiles firstUnit distGrid units

