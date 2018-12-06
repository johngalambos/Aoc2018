type Extents =
  { xmin: int
    xmax: int
    ymin: int
    ymax: int }

type TileValue = Tie of int | Closest of int*int

type Coord = int*int

type CoordDist =
  { coord: Coord
    dist: int }

let testCoords = [ (1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9) ]

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

let extents = findExtents testCoords

// overlayCoordGrid :: Map<Coord, TileValue> -> (CoordDist) -> Map<Coord, TileValue>
let overlayCoordGrid map coordGrid coordId =
  //fold over coordgrid popluting the map
  Seq.fold
    (fun (grid:Map<Coord, TileValue>) c ->
      match Map.containsKey c.coord grid with
      | true ->
        match Map.find c.coord grid with
        | Tie d when d > c.dist -> Map.add c.coord (Closest (c.dist, coordId)) grid
        | Closest (d, _) when d > c.dist -> Map.add c.coord (Closest (c.dist, coordId)) grid
        | Closest (d, _) when d = c.dist -> Map.add c.coord (Tie c.dist) grid
        | _ -> grid
      | false ->
        Map.add c.coord (Closest (c.dist, coordId)) grid)
    map
    coordGrid


let caps = ['A' .. 'F']
let lowers = ['a' .. 'f']
//fold over the list of coords using ints as ids
testCoords
|> List.mapi (fun i c -> (i, (createCoordGrid extents c)))
|> List.fold
  (fun map (i, cGrid) -> overlayCoordGrid map cGrid i)
  Map.empty
|> Map.toList
|> List.map (fun item ->
  let (c, tv) = item
  match tv with
  | Closest (0, id) ->
    printf "%s" (caps.[id].ToString())
  | Closest (d, id) ->
    printf "%s" (lowers.[id].ToString())
  | Tie _ ->
    printf "."
  match c with
  | (_, 9) -> printfn ""
  | _ -> printf ""
  item)
|> List.map snd
|> List.filter (fun x ->
  match x with
  | Closest _ -> true
  | _ -> false)
|> List.groupBy (fun (Closest (id, _)) -> id)
|> List.map (fun (g, closests) -> (g, List.length closests))
|> List.maxBy (fun (g, closestTilesCount) -> closestTilesCount)




//create an empty grid to hold the results


// let resultGrid = overlay
//overlay the sequences on each other keeping only the shortest manhattan
//distance for each square or ignoring  ties (by using None). Remember later coordinates
//could possibly break ties
// grid
// |> Seq.iter (fun x -> printfn "%A" x)
