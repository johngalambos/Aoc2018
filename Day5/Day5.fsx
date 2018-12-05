let inputTest = "dabAcCaCBAcCcaDA"
let inputActual = System.IO.File.ReadAllText(@"C:\Users\bil91\code\Aoc2018\Day5\input.txt")

let isReversePolarity (x:char, y:char) =
  if x.ToString().ToUpper() = y.ToString().ToUpper() && x <> y then
    true
  else
    false

let rec react coll nexts =
  match (coll, nexts) with
  | (_, []) -> coll
  | (hColl::tColl, hNext::tNext) when isReversePolarity (hColl, hNext) ->
      react tColl tNext
  | (hColl::tColl, hNext::tNext) -> react (hNext::hColl::tColl) tNext
  | ([], hNext::tNext) -> react [hNext] tNext

let collapsed = react [] (List.ofArray (inputActual.ToCharArray()))
List.length collapsed //answer for part 1

let units = ['a' .. 'z']

let filterUnit (letter: char) (input: System.String) =
  let noLower = input.Replace (letter.ToString().ToLower(), "")
  noLower.Replace (letter.ToString().ToUpper(), "")

units
|> List.map (fun n -> (n, react [] (Seq.toList (filterUnit n inputActual))))
|> List.map (fun (n, reacted) -> (n, List.length reacted))
|> List.minBy (fun (n, len) -> len)
|> printfn "%A" //answer for part 2




