open System.Collections.Generic


type OptionType  =  Optional | Mandatory
type Segment = SingleSegment of char list | ForkedSegment of SegmentOption
and SegmentOption = { Options: Segment list list ; Type: OptionType }

let toString chars =
  System.String(chars |> Array.ofList)

let rec parseOptionsSegment (segChars, currentOptionSegments, allOptions) = function
  | '(' :: chars ->
    if segChars <> [] then
      let prevSeg = SingleSegment (segChars |> List.rev)
      let childSegment, chars = parseOptionsSegment ([], [], []) chars
      parseOptionsSegment ([], childSegment::prevSeg::currentOptionSegments, allOptions) chars
    else
      let childSegment, chars = parseOptionsSegment ([], [], []) chars
      parseOptionsSegment ([], childSegment::currentOptionSegments, allOptions) chars
  | '|'::')'::chars ->
    if segChars <> [] then
      let currentOption = SingleSegment (segChars |> List.rev) :: currentOptionSegments
      let allOptions = currentOption :: allOptions |> List.rev
      (ForkedSegment { Options = allOptions; Type = Optional }, chars)
    else
      (ForkedSegment { Options = currentOptionSegments::allOptions |> List.rev; Type = Optional }, chars)
  | '|'::chars ->
    //a single option can be a list of segments
    let currentOption = SingleSegment (segChars |> List.rev) :: currentOptionSegments |> List.rev
    parseOptionsSegment ([], [], (currentOption::allOptions)) chars
  | ')'::chars ->
    if segChars <> [] then
      let currentOption = SingleSegment (segChars |> List.rev) :: currentOptionSegments |> List.rev
      let allOptions = currentOption :: allOptions |> List.rev
      (ForkedSegment { Options = allOptions; Type = Mandatory }, chars)
    else
      let currentOption = currentOptionSegments |> List.rev
      let allOptions = currentOption::allOptions |> List.rev
      (ForkedSegment { Options = allOptions; Type = Mandatory }, chars)
  | c::chars->
    parseOptionsSegment (c::segChars, currentOptionSegments,  allOptions) chars
  | [] -> failwith "unexpected end of input"

let rec parseSegments acc chars = seq {
  let emitCurrentSegment = seq {
    if acc <> [] then
      yield  SingleSegment (acc |> List.rev) }

  match chars with
  | '^':: chars ->
    yield! parseSegments acc chars
  | '$':: chars ->
    yield! emitCurrentSegment
  | '('::chars ->
    yield! emitCurrentSegment
    let (segment, chars) = parseOptionsSegment ([],[], []) chars
    yield segment
    yield! parseSegments [] chars
  | c::chars -> yield! parseSegments (c::acc) chars
  | [] -> failwith "unexpected end of input" }

let testParse (input:string) =
  let parsed = parseSegments [] (input.ToCharArray() |> List.ofArray)
  parsed
  |> Seq.iter (fun s -> printfn "%A" s)

type Dir = Left | Right | Up | Down

type Room (location) =
  let location = location
  let connections = new ResizeArray<Room>()

  let getLocation (x, y) dir =
    match dir with
    | Down -> (x, y - 1)
    | Up -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

  member r.Location = location

  member r.HasDoor dir =
    connections.Exists (fun c -> c.Location = getLocation location dir)

  member r.AddConnection (dest:Room) =
    if connections.Exists (fun c -> c.Location = dest.Location) then
      ()
    else
      connections.Add(dest)

type ElfMap() =
  let rooms = new Dictionary<int*int, Room>()

  member m.AddRoom (room:Room) =
    rooms.Add(room.Location, room)

  member m.AddConnection src dest =
    let srcRoom =
      if rooms.ContainsKey(src) then
        rooms.[src]
      else
        let newRoom = new Room(src)
        rooms.Add(src, newRoom)
        newRoom

    let destRoom =
      if rooms.ContainsKey(dest) then
        rooms.[dest]
      else
        let newRoom = new Room(dest)
        rooms.Add(dest, newRoom)
        newRoom

    srcRoom.AddConnection destRoom
    destRoom.AddConnection srcRoom

  member m.Print () =

    //#####
    //#.|.#
    //#-#-#
    //#.|.#
    //#####
    //for a map n rooms wide we need an extra two width for the border
    //and n - 1 tiles for the doors
    //same goes for height
    //each tile is 1 + x
    //walls are between rooms unless otherwise stated
    //for each room check if there is a door to the right or down
    // we'll print those
    let isEven n = (n % 2 = 0)
    let isOdd n =  not (isEven n)
    let xs = rooms.Keys |> Seq.map (fun (x, y) -> x)
    let ys = rooms.Keys |> Seq.map (fun (x, y) -> y)
    let xMin = Seq.min xs
    let xMax = Seq.max xs
    let yMin = Seq.min ys
    let yMax = Seq.max ys
    let offsetX = 0 - xMin
    let offsetY = 0 - yMin
    let width = (xMax - xMin + 1)
    let height = (yMax - yMin + 1)
    let gridWidth = (2 * width + 1)
    let gridHeight = (2 * height + 1)
    let getIJ (x, y) =
      ((y + offsetY) * 2 - 1, (x + offsetX) * 2 - 1)

      // a j of 0 should be the max Y coord
      // max j should be the min Y coord
      // -2 y should be an i of 10
      // 2 y should be an i of 1
      // minY should be i of 1

    let getXY (i, j) =
      let loc = ((j + 1) / 2 - 1 - offsetX),  -1 * (i + 1) / 2 + height + yMin
      // printfn "converting i,j (%i, %i) to x,y %A" i j loc
      loc


    printfn "xMin %i yMin %i xMax %i yMax %i width %i height %i gridWidth %i gridWidth %i" xMin yMin xMax yMax width height gridWidth gridHeight
    for i in 0..gridHeight - 1 do
      for j in 0..gridWidth - 1 do
        match (i, j) with
        | i, _ when i = 0 || i = gridWidth - 1 -> printf "#"
        | _, j when j = 0 || j = gridWidth - 1  -> printf "#"
        | i, j when isEven i && isEven j -> printf "#"
        | i, j when isEven i && isOdd j ->
          let (x,y) = getXY(i - 1, j)
          if rooms.ContainsKey (x, y) && rooms.Item((x,y)).HasDoor(Down) then
            printf "-"
          else
            printf "#"
        | i, j when isOdd i && isEven j ->
          let (x,y) = getXY(i, j - 1)
          if rooms.ContainsKey (x, y) && rooms.Item((x,y)).HasDoor(Right) then
            printf "|"
          else
            printf "#"
        | (i, j) when getXY (i, j) = (0, 0) -> printf "X"
        | i, j when rooms.ContainsKey(getXY (i, j)) -> printf "."
        | _ -> printf "#"
      printfn ""






let getDest (x, y) direction =
  match direction with
  | 'N' -> (x, y + 1)
  | 'E' -> (x + 1, y)
  | 'S' -> (x, y - 1)
  | 'W' -> (x - 1, y)
  | _ -> failwith "unexpected direction"

let rec walkRoomString (map:ElfMap) (x, y) = function
  | [] -> (x, y)
  | c::chars ->
    printfn "walking %c from %A" c (x, y)
    let dest = getDest (x, y) c
    map.AddConnection (x, y) dest
    walkRoomString map dest chars


let rec walkSegment segment startingPoint map =
    match segment with
    | SingleSegment dirs ->
        let endPoint = walkRoomString map startingPoint dirs
        printfn "end point of single segment walk %A starting from %A was %A" dirs startingPoint endPoint
        endPoint
    | ForkedSegment fs ->
        //walk the options in parallel from each starting point; their endpoints should be identical
        let nextStartingPoints =
          fs.Options
          |> Seq.fold (fun acc routeOption ->
            let segEndPoint = walkSegments routeOption startingPoint map
            // printfn "end point of forked segment walk %A starting from %A was %A" fs startingPoint segEndPoint
            segEndPoint::acc) []

        printfn "fs end points. Should be unique %A" nextStartingPoints
        match fs.Type with
        | Mandatory -> List.head nextStartingPoints
        | Optional ->
          if List.head nextStartingPoints <> startingPoint then
            failwith "expected  to be identical"
          else
            List.head nextStartingPoints

and walkSegments segments startingPoint map =
  printfn "walking a list of segments from starting points %A" startingPoint
  let finalCoord =  segments |> Seq.fold (fun coord seg -> walkSegment seg coord map) startingPoint
  finalCoord


// let input = "^ENWWW(NEEE|SSE(EE|N))$"
// let input = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
// let input = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
// let input = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
let input = System.IO.File.ReadAllText("C:\Users\john\code\Aoc2018\Day20\input.txt")

let parsed = parseSegments [] (input.ToCharArray() |> List.ofArray)

let genMap parsed =
  let map = new ElfMap()
  let finalCooord = walkSegments parsed (0,0) map
  printfn "final coord %A" finalCooord
  map

let map = genMap (parsed |> Seq.toList)
map.Print()

