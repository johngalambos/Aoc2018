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
            dist = abs (x - i) + abs (j - y) } }


// overlayCoordGrid :: Map<Coord, TileValue> -> (CoordDist) -> Map<Coord, TileValue>
let overlayCoordGrid map coordGrid coordId =
  //fold over coordgrid popluting the map
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



let lines = System.IO.File.ReadLines(@"C:\Users\bil91\code\Aoc2018\Day6\input.txt")
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

coords
|> List.mapi (fun i c -> (i, (createCoordGrid extents c)))
|> List.fold
  (fun map (i, cGrid) -> overlayCoordGrid map cGrid i)
  Map.empty
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
|> List.maxBy (fun (_, closestTilesCount) -> closestTilesCount)


//to visualize
// let testCoords = [ (1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9) ]
// let extents = findExtents testCoords
// let caps = ['A' .. 'F']
// let lowers = ['a' .. 'f']

// testCoords
// |> List.mapi (fun i c -> (i, (createCoordGrid extents c)))
// |> List.fold
//   (fun map (i, cGrid) -> overlayCoordGrid map cGrid i)
//   Map.empty
// |> Map.toList
// |> List.sortBy (fun ((x,y), _) -> (y, x))
// |> List.map (fun item ->
//   let (c, tv) = item
//   match tv with
//   | Closest cc  when cc.Dist = 0 ->
//     printf "%s" (caps.[cc.Id].ToString())
//   | Closest cc ->
//     printf "%s" (lowers.[cc.Id].ToString())
//   | Tie _ ->
//     printf "."
//   match c with
//   | (8, _) -> printfn ""
//   | _ -> printf ""
//   item)
// |> List.map snd
// |> List.choose (fun x ->
//   match x with
//   | Closest cc -> Some cc
//   | _ -> None)
// |> List.groupBy (fun cc -> cc.Id)
// |> List.map (fun (g, closests) -> (g, List.length closests))
// |> List.map (fun x ->
//   printfn "%A" x
//   x)
// |> List.maxBy (fun (_, closestTilesCount) -> closestTilesCount)


