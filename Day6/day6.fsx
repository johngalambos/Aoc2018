open System

type Extents =
  { xmin: int
    xmax: int
    ymin: int
    ymax: int }

type ClosestCandidate =
  { Id: int
    Dist: int }


type TileValue = Tie of int | Closest of ClosestCandidate

type Coord = int*int

type CoordDist =
  { coord: Coord
    dist: int }

let findExtents coords =
  let xmin = fst ((List.minBy fst) coords)
  let xmax = fst ((List.maxBy fst) coords)
  let ymin = snd ((List.minBy snd) coords)
  let ymax = snd ((List.maxBy snd) coords)
  { xmin = xmin
    xmax = xmax
    ymin = ymin
    ymax = ymax }

// populate the extents with the manhattan distance for a single coord
// 2 dimensional list
let createCoordGrid extents (x, y) =
  seq {
    for i in extents.xmin .. extents.xmax do
      for j in extents.ymin .. extents.ymax do
        yield
          { coord =  (i, j)
            dist = abs (i - x) + abs (j - y) } }


// overlayCoordGrid :: Map<Coord, TileValue> -> (CoordDist) -> Map<Coord, TileValue>
let overlayCoordGrid map coordGrid coordId =
  //fold over coordgrid populating the map
  Seq.fold
    (fun (grid:Map<Coord, TileValue>) cand ->
      match Map.containsKey cand.coord grid with
      | true ->
        match Map.find cand.coord grid with
        | Tie d when d > cand.dist ->
            Map.add cand.coord (Closest { Dist = cand.dist; Id = coordId }) grid
        | Closest current  when current.Dist > cand.dist ->
            Map.add cand.coord (Closest { Dist = cand.dist; Id = coordId }) grid
        | Closest current  when current.Dist = cand.dist ->
            Map.add cand.coord (Tie cand.dist) grid
        | _ -> grid
      | false ->
        Map.add cand.coord (Closest { Dist = cand.dist; Id = coordId } ) grid)
    map
    coordGrid



let lines = System.IO.File.ReadLines(@"C:\Users\john\code\Aoc2018\Day6\input.txt")
// let lines = System.IO.File.ReadLines(@"C:\Users\john\code\Aoc2018\Day6\input2.txt")
// let lines = System.IO.File.ReadLines(@"C:\Users\john\code\Aoc2018\Day6\inputSample.txt")

let coords =
  lines
  |> Seq.map (fun x ->
    printfn "%A" x
    let components = x.Split([|", "|], StringSplitOptions.None)
    (components.[0] |> int, components.[1] |> int))
  |> List.ofSeq

let extents = findExtents coords
// let testCoords = [ (1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9) ]
// let extents = findExtents testCoords

let results =
  coords
  |> List.mapi (fun i c -> (i, (createCoordGrid extents c)))
  |> List.fold
    (fun map (i, cGrid) -> overlayCoordGrid map cGrid i)
    Map.empty

let finalCoords =
  results
  |> Map.toList
  |> List.map fst

let minX =
  finalCoords
  |> List.minBy fst
  |> fst

let minY =
  finalCoords
  |> List.minBy snd
  |> snd

let maxX =
  finalCoords
  |> List.maxBy fst
  |> fst

let maxY =
  finalCoords
  |> List.maxBy snd
  |> snd



let edgeAreas =
  results
  |> Map.toList
  |> List.filter (fun ((x, y), item) ->
    (x = minX || y = minY || x = maxX || y = maxY))
  |> List.choose (fun (_, item) ->
    match item with
    | Closest c -> Some c
    | _ -> None)
  |> List.map (fun c -> c.Id)
  |> List.distinct



let maxBoundedArea =
  results
  |> Map.toList
  |> List.map snd
  |> List.choose (fun x ->
    match x with
    | Closest cc -> Some cc
    | _ -> None)
  |> List.groupBy (fun cc -> cc.Id)
  |> List.map (fun (id, closests) -> (id, List.length closests))
  |> List.map (fun x ->
    printfn "%A" x
    x)
  |> List.filter (fun (id, count) -> not (List.contains id edgeAreas))
  |> List.maxBy (fun (_, closestTilesCount) -> closestTilesCount)

//we can't allow one on the edge


let greekCaps = ['Α'; 'Β'; 'Γ'; 'Δ'; 'Ε'; 'Ζ'; 'Η'; 'Θ'; 'Ι'; 'Κ'; 'Λ'; 'Μ'; 'Ν'; 'Ξ'; 'Ο'; 'Π'; 'Ρ'; 'Σ'; 'Τ'; 'Υ'; 'Φ'; 'Χ'; 'Ψ'; 'Ω']
let greekLowers = ['α'; 'β'; 'γ'; 'δ'; 'ε'; 'ζ'; 'η'; 'θ'; 'ι'; 'κ'; 'λ'; 'μ'; 'ν'; 'ξ'; 'ο'; 'π'; 'ρ'; 'σ'; 'τ'; 'υ'; 'φ'; 'χ'; 'ψ'; 'ω']
let caps = ['A' .. 'Z'] @ greekCaps
let lowers = ['a' .. 'z'] @ greekLowers

let toDisplay results =
  results
  |> Map.toList
  |> List.groupBy (fun ((x,y), _) -> y)
  |> List.sortBy (fun (y, _) -> y)
  |> List.map (fun (y, lineItems) ->
    lineItems
    |> List.map (fun item ->
      let (c, tv) = item
      match tv with
      | Closest cc  when cc.Dist = 0 ->
        caps.[cc.Id]
      | Closest cc ->
        lowers.[cc.Id]
      | Tie _ -> '.'))
  |> List.map (fun lineChars ->
    String.concat "" <| List.map string lineChars)

System.IO.File.WriteAllLines(@"C:\Users\john\code\Aoc2018\Day6\output2.txt", results |> toDisplay)
