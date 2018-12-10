type GameState =
  { CurrentMarbleIndex: int
    Board: int list
    ScoreBoard: Map<int, int> }

let insert (index: int) (value:int) (list: int list) =
  let loopIndex = index % List.length list
  let newList = list.[0 .. loopIndex - 1] @ [value] @ list.[loopIndex..]
  newList, loopIndex


let pluck (index: int) (list: int list) =
  let actualIndex =
    match index with
    | i when i < 0 -> (List.length list) + i
    | i -> i

  let plucked = list.[actualIndex]
  let newList = list.[0 .. actualIndex - 1] @ list.[actualIndex + 1..]
  (newList, actualIndex, plucked)


let playNormal state marble =
  let board = state.Board
  let cmi = state.CurrentMarbleIndex
  let nextMarbleIndex = cmi + 2
  let board, cmi = insert nextMarbleIndex marble board
  { state with CurrentMarbleIndex = cmi; Board = board }


let play23 state marble player =
  let board = state.Board
  let cmi = state.CurrentMarbleIndex
  let playerScore = Map.find player state.ScoreBoard
  let (board, cmi, plucked) = pluck (cmi - 7) board
  let newScore = Map.add player (playerScore + marble + plucked) state.ScoreBoard
  { state with CurrentMarbleIndex = cmi; Board = board; ScoreBoard = newScore }


let play playerCount state marble =
  if marble % 10000 = 0 then
    printfn "marble %A" marble

  let player = ((marble - 1) % playerCount + 1)
  // printfn "player %A is playing marble %A" player marble
  match marble % 23 with
  | 0 -> play23 state marble player
  | _ -> playNormal state marble


let go playerCount marbleCount =
  let startingScoreBoard =
    { 1 .. playerCount }
    |> Seq.fold (fun score player -> Map.add player 0 score) Map.empty

  let startingBoard = [0]

  let initialState =
    { CurrentMarbleIndex = 0
      Board = startingBoard
      ScoreBoard = startingScoreBoard }

  { 1 .. marbleCount }
  |> Seq.fold (play playerCount) initialState


let getHighScore players marbles =
  let result = go players marbles
  result.ScoreBoard |> Map.toList |> List.maxBy snd


// getHighScore 9 25
// getHighScore 10 1618
// getHighScore 13 7999
getHighScore 17 1104
// getHighScore 21 6111
// getHighScore 424 71482



