
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

let lastScoresAsText length (scores:ResizeArray<int>) offset =
  match length with
  | 1 ->
    scores.[scores.Count - offset - 1] |> string
  | _ ->
    let fromI = (scores.Count - length - offset)
    let toI = (scores.Count - offset - 1)
    [fromI..toI]
    |> Seq.map (fun i -> scores.[i] |> string)
    |> String.concat ""

let lastScoresMatchGoal (scores:ResizeArray<int>) goal offset =
  // printfn "attmpeting last score with %A scores and goal length %A" scores.Count (Seq.length goal)
  if scores.Count < ((Seq.length goal) + 1)  then
    false
  else
    let lsText = lastScoresAsText (Seq.length goal) scores offset
    // printfn "comparing ls %A to goal %A using offset %A" lsText goal offset
    lsText = goal

let rec iterate elfState (scores:ResizeArray<int>) goal itCount =
  if itCount % 1000000 = 0 then
    printfn "iteration %A" itCount
  match goal with
  | _ when
    lastScoresMatchGoal scores goal 0 ->
      scores.Count - (Seq.length goal)
  | _ when
    lastScoresMatchGoal scores goal 1 ->
      scores.Count - (Seq.length goal) - 1
  | _ ->
    let newRecipe = getNewRecipe elfState
    for r in newRecipe do
      scores.Add(r)
    let elfState = elfStep  elfState scores
    iterate elfState scores goal (itCount + 1)

let solve input =
  let initialState = ((3, 0), (7, 1))
  let scores = new ResizeArray<int>()
  scores.Add(3)
  scores.Add(7)
  let recipeCount = iterate initialState scores (input |> string) 0

  printfn "previous recipes %i" recipeCount

solve 990941
