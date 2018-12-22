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
    printfn "target %A" (it, jt)
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
    | _ -> failwith  "screwed up"

  describeRegion



let riskLevel (map: Region[,]) =
  seq { for i in 0..Array2D.length1 map - 1 do
          for j in 0..Array2D.length2 map - 1 do
            let risk =
                 match map.[i,j].regionType with
                  | Rocky -> 0
                  | Wet -> 1
                  | Narrow -> 2
            yield risk }
  |> Seq.sum



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

let sampleMouth = (0, 0)
let target = (10, 10)
let depth = bigint 510

let describeRegion = createInitializer depth target

// let target = (757, 12)
// let depth = bigint 3198

let map = genMap describeRegion target
printMap map
let risk = riskLevel map
