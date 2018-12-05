open System.Text.RegularExpressions


let input1 = "#123 @ 3,2: 5x4"

type Claim  =
    { Id: int
      offsetX: int
      offsetY: int
      width: int
      height: int }

// parseInput string -> claim
let parseInput (input: string) =
    let result = Regex.Match(input, "^\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$")
    { Id = result.Groups.[1].Value |> int
      offsetX = result.Groups.[2].Value |> int
      offsetY = result.Groups.[3].Value |> int
      width = result.Groups.[4].Value |> int
      height = result.Groups.[5].Value |> int }


parseInput input1

