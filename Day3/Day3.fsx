open System.Text.RegularExpressions


let input1 = "#123 @ 3,2: 5x4"
let input2 = "#123 @ 3,2: 5x4"

let inputsTest =
  seq {
    yield input1
    yield input2 }

let inputsActual = System.IO.File.ReadLines(@"C:\Users\john\code\Aoc2018\Day3\input.txt")

type Claim  =
  { Id: int
    offsetX: int
    offsetY: int
    width: int
    height: int }

// parseInput :: string -> Claim
let parseInput (input: string) =
  let result = Regex.Match(input, "^\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$")
  { Id = result.Groups.[1].Value |> int
    offsetX = result.Groups.[2].Value |> int
    offsetY = result.Groups.[3].Value |> int
    width = result.Groups.[4].Value |> int
    height = result.Groups.[5].Value |> int }


// toCoords :: Claim -> seq of Claim
let toCoords claim =
  seq { for i in 0 .. claim.width - 1 do
          for j in 0 .. claim.height - 1 do
            yield (claim.offsetX + i, claim.offsetY + j) }


let overlay map coord =
  match Map.containsKey coord map with
  | true ->
    Map.add coord ((Map.find coord map) + 1) map
  | false ->
    Map.add coord 1 map

// overlayCoords :: seq<int*int> -> Map -> Map
let overlayCoords map coords =
  Seq.fold overlay map coords

//from the sequence of inputs get all coords
let getMap inputs =
  let coords =
    seq { for input in inputs do
            yield! parseInput input |> toCoords }

  coords
  |> overlayCoords Map.empty

let tapestry = getMap inputsActual

// Answer for Part 1

tapestry
|> Map.filter (fun _ total -> total > 1)
|> Map.count


// Part 2
// now that we now where everything is, if we subtract a claim from the map and
// it leaves a hole of zero values then that's the patch we're looking for
// this means it covers any area where the totals are all 1

// subtract :: map -> seq<int*int> -> seq<int*int>
let claimsCoords =
  inputsActual
  |> Seq.map parseInput
  |> Seq.map (fun c -> (c.Id, toCoords c))

let hasNoOverlaps coords tapestry =
  Seq.forall (fun coord -> (Map.find coord tapestry) = 1)


Seq.find (fun (id, coords) ->  hasNoOverlaps coords tapestry) claimsCoords






