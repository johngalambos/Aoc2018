open System.Collections.Generic

type PlacedMarble =
  { Id: int
    CwNext: int
    CcwNext: int }

type CircularList =
  { Marbles: Dictionary<int, PlacedMarble>
    Current: PlacedMarble }

  member c.Add marbleId =
    match c.Marbles.Count with
    | 1 ->
      let single = c.Current
      let single = { single with CwNext = marbleId
                                 CcwNext = marbleId }
      let nm = { Id = marbleId
                 CwNext = single.Id
                 CcwNext = single.Id }

      c.Marbles.[single.Id] <- single
      c.Marbles.Add (nm.Id, nm)
      { c with Current  = nm }
    | _ ->
      let before = c.Current
      let after =  c.FindMarble c.Current.CwNext
      let before = { before with CwNext = marbleId }
      let after = { after with CcwNext = marbleId }
      let nm = { Id = marbleId
                 CwNext = after.Id
                 CcwNext = before.Id }

      c.Marbles.[before.Id] <- before
      c.Marbles.[after.Id] <- after
      c.Marbles.Add (nm.Id, nm)
      { c with Current  = nm }

  member c.RemoveCurrent =
    let before = c.FindMarble c.Current.CcwNext
    let after = c.FindMarble c.Current.CwNext
    let before = { before with CwNext = after.Id }
    let after = { after with CcwNext = before.Id }
    c.Marbles.Remove (c.Current.Id) |> ignore
    c.Marbles.[before.Id] <- before
    c.Marbles.[after.Id] <- after
    { c with Current  = after }


  member c.FindMarble id =
    c.Marbles.[id]

  member c.MoveClockise =
    { c with Current = c.FindMarble c.Current.CwNext }

  member c.MoveCounterClockwise =
    { c with Current = c.FindMarble c.Current.CcwNext }

  member c.MoveCounterClockwiseN n =
    match n with
    | 0 -> c
    | _ ->
      {1..n}
      |> Seq.fold (fun c n -> c.MoveCounterClockwise) c

  static member create =
    let nm =
      { Id = 0
        CwNext = 0
        CcwNext = 0 }
    let dict = new Dictionary<int, PlacedMarble>()
    dict.Add (0, nm)

    { Marbles = dict
      Current = nm }


type GameState =
  { Board: CircularList
    ScoreBoard: Map<int, int64> }

let playNormal state marble =
  let board = state.Board
  let board = board.MoveClockise
  let board = board.Add marble
  { state with Board = board }


let play23 state marble player =
  let board = state.Board
  let playerScore = Map.find player state.ScoreBoard
  let board = board.MoveCounterClockwiseN 7
  let plucked = board.Current.Id
  let board = board.RemoveCurrent
  let newScore = Map.add player (playerScore + int64(marble) + int64(plucked)) state.ScoreBoard
  { state with Board = board; ScoreBoard = newScore }


let play playerCount state marble =
  // if marble % 10000 = 0 then
  //   printfn "marble %A" marble

  let player = ((marble - 1) % playerCount + 1)
  // printfn "player %A is playing marble %A" player marble
  match marble % 23 with
  | 0 -> play23 state marble player
  | _ -> playNormal state marble


let go playerCount marbleCount =
  let startingScoreBoard =
    { 1 .. playerCount }
    |> Seq.fold (fun score player -> Map.add player (int64(0)) score) Map.empty

  let startingBoard = CircularList.create

  let initialState =
    { Board = startingBoard
      ScoreBoard = startingScoreBoard }

  { 1 .. marbleCount }
  |> Seq.fold (play playerCount) initialState


let getHighScore players marbles =
  let result = go players marbles
  result.ScoreBoard |> Map.toList |> List.maxBy snd


// getHighScore 9 25
// getHighScore 10 1618
// getHighScore 13 7999
// getHighScore 17 1104
// getHighScore 21 6111
//getHighScore 424 71482
getHighScore 424 7148200



