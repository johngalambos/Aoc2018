open System
open System.Text.RegularExpressions

type Point =
  { x: int
    y: int
    vX: int
    vY: int }

let parsePoint line =
  let result = Regex.Match(line, "position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>")
  { x  = result.Groups.[1].Value |> int
    y  = result.Groups.[2].Value |> int
    vX = result.Groups.[3].Value |> int
    vY = result.Groups.[4].Value |> int }

let tickPoint boost point =
  { point with x = point.x + (point.vX * boost)
               y = point.y + (point.vY * boost)}

let boundingBox (points:seq<Point>) =
  let xMin = Seq.minBy (fun p -> p.x) points
  let xMax = Seq.maxBy (fun p -> p.x) points
  let yMin = Seq.minBy (fun p -> p.y) points
  let yMax = Seq.maxBy (fun p -> p.y) points
  (xMin.x, yMin.y, xMax.x, yMax.y)

let boundingArea (bbox: int*int*int*int) =
  let (xMin, yMin, xMax, yMax) = bbox
  abs(int64(xMax) - int64(xMin)) * abs(int64(yMax) - int64(yMin))

let isCoord points x y =
  Seq.exists (fun p -> p.x = x && p.y = y) points

let createLineOutput y points xMin xMax =
  seq { xMin - 5 .. xMax + 5 }
  |> Seq.fold (fun line x ->
    if isCoord points x y then
      line + "#"
    else
      line + "." ) ""

let printSnapshot points bbox =
  let (xMin, yMin, xMax, yMax) = bbox
  let lines =
    seq { yMin - 5 .. yMax + 5 }
    |> Seq.map (fun y -> createLineOutput y points xMin xMax)
  lines
  |> Seq.iter (fun l -> printfn "%s" l)


let jumpTick points startingAt =
  points
  |> Seq.map (tickPoint startingAt)

let rec tick points interval =
  let newPositions =
    points
    |> Seq.map (tickPoint 1)
  let bbox = boundingBox newPositions
  let boundArea = boundingArea bbox
  if interval % 50 = 0 then
    printfn "tick %A boundArea %A" interval boundArea
  if boundArea < int64(2000) then
    printfn "tick %A boundArea %A bbox %A" interval boundArea bbox
    printSnapshot points bbox
  match interval with
  | i when i >= 15000 -> newPositions | _ -> tick newPositions (interval + 1)

let getInput path =
  System.IO.File.ReadLines(path)
  |> Seq.map parsePoint

[<EntryPoint>]
let main argv =
  // let boost = 2
  // let inputFile = "/Users/john/code/Aoc2018/Day10/inputSample.txt"
  let boost = 10300
  let inputFile = "/Users/john/code/Aoc2018/Day10/input.txt"
  let points = getInput inputFile
  let boostedPoints = jumpTick points boost
  let output = tick boostedPoints boost
  printfn "done"
  0
