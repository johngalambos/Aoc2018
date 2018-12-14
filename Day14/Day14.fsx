
let elfStep elfState (scores:ResizeArray<int>) =
  let ((r0, i0), (r1, i1)) = elfState
  let i0' = (i0 + r0 + 1) % scores.Count
  let i1' = (i1 + r1 + 1) % scores.Count
  let r0' = scores.[i0']
  let r1' = scores.[i1']
  ((r0', i0'), (r1', i1'))

let getNewRecipe elfState =
  let ((r0, _), (r1, _)) = elfState
  if r0 + r1 > 9 then
    let str = sprintf "%i" (r0 + r1)
    let num1 = str.[0] |> string |> int
    let num2 = str.[1] |> string |> int
    [num1; num2]
  else
    [r0 + r1]

let rec iterate elfState (scores:ResizeArray<int>) finishScoreN =
  match finishScoreN with
  | n when scores.Count >= n -> scores
  | _ ->
    let newRecipe = getNewRecipe elfState
    for r in newRecipe do
      scores.Add(r)
    let elfState = elfStep  elfState scores
    iterate elfState scores finishScoreN

let solve input =
  let nextRecipeN = 10
  let initialState = ((3, 0), (7, 1))
  let scores = new ResizeArray<int>()
  let targetScoreCount = input + nextRecipeN
  scores.Add(3)
  scores.Add(7)
  let finalScores = iterate initialState scores targetScoreCount

  let  lastTen =
    if finalScores.Count > targetScoreCount then
      [finalScores.Count - 11..finalScores.Count - 2]
      |> Seq.map (fun i -> finalScores.[i])
    else
      [finalScores.Count - 10..finalScores.Count - 1]
      |> Seq.map (fun i -> finalScores.[i])

  let lastTenText =
    lastTen
    |> Seq.map string
    |> String.concat ""

  printfn "score %s" lastTenText


solve 9
