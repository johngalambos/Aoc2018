type Extents =
  { xmin: int
    xmax: int
    ymin: int
    ymax: int }

let testCoords =
  [ (1, 1);
    (1, 6);
    (8, 3);
    (3, 4);
    (5, 5);
    (8, 9) ]

let findExtents coords =
  let xmin = fst ((List.minBy fst) coords)
  let xmax = fst ((List.maxBy fst) coords)
  let ymin = snd ((List.minBy snd) coords)
  let ymax = snd ((List.maxBy snd) coords)
  { xmin = xmin
    xmax = xmax
    ymin = ymin
    ymax = ymax }


printfn "%A" (findExtents testCoords)

// populate the extents with the manhattan distance for a single coord
// 2 dimensional list
let createGrid extents (x, y) =
  seq {
    for i in extents.xmin .. extents.ymax do
      for j in extents.ymin .. extents.ymax do
        yield (i, j, abs (x - i) + abs (y - j)) }

let extents = findExtents testCoords

let grid = createGrid extents (8, 3)

//overlay the sequences on each other keeping only the shortest manhattan
//distance for each square or ignoring  ties (by using None). Remember later coordinates
//could possibly break ties
grid
|> Seq.iter (fun x -> printfn "%A" x)
