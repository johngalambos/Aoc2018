open System

type Extents =
  { xmin: int
    xmax: int
    ymin: int
    ymax: int }

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

let createCoordGrid extents landingCoords =
  seq {
    for i in extents.xmin .. extents.xmax do
      for j in extents.ymin .. extents.ymax do
        let totalDist = List.fold (fun accDist landingCoord ->
          let (lx, ly) = landingCoord
          accDist + abs(i - lx) + abs(j - ly)) 0 landingCoords

        yield
          { coord = (i, j)
            dist = totalDist } }




let lines = System.IO.File.ReadLines(@"C:\Users\john\code\Aoc2018\Day6\input.txt")
let maxDistance = 10000
// let lines = System.IO.File.ReadLines(@"C:\Users\john\code\Aoc2018\Day6\inputSample.txt")
// let maxDistance = 32

let coords =
  lines
  |> Seq.map (fun x ->
    printfn "%A" x
    let components = x.Split([|", "|], StringSplitOptions.None)
    (components.[0] |> int, components.[1] |> int))
  |> List.ofSeq

let extents = findExtents coords

let results =
  createCoordGrid extents coords
  |> Seq.filter (fun cd -> cd.dist < maxDistance)
  |> Seq.length

