module Tiler exposing
    ( generateBoard
    , Neighbor(..)
    , positionToString
    , positionFromString
    , positionToXY
    , Board
    , emptyBoard
    , map
    )


{-| Generate random tiling patters.

    import Tiler exposing (Board, Neighbor(..), emptyBoard, generateBoard, map)

# Tiler

@docs Board, emptyBoard, generateBoard, map, Neighbor, positionToString, positionFromString, positionToXY
-}


import Dict exposing (Dict)
import Random exposing (Generator, Seed)


{-| Simple type for storing the positions of tiles in the Board as well as being able to identify neighbors.
-}
type alias Position =
    { x : Int
    , y : Int
    --  NOTE: For future 3D tiling
    -- , z : Int
    }


{-| Used for pretty printing a Position.

    ```
    -- A position where x == 3 and y ==2
    positionToString pos == "3, 2"
    ```
-}
positionToString : Position -> String
positionToString { x, y } =
    (String.fromInt x) ++ ", " ++ (String.fromInt y)


{-| Used to build a position from a string

    ```
    positionFromString "3, 2" == Just { x = 3, y = 2 }
    positionFromString "6 8" == Nothing
    ```
-}
positionFromString : String -> Maybe Position
positionFromString s =
    case String.split ", " s of
        (x :: y :: []) ->
            let
                newX =
                    case String.toInt x of
                        Nothing -> 0
                        Just iX -> iX
                newY =
                    case String.toInt y of
                        Nothing -> 0
                        Just iY -> iY
            in
                Just <| Position newX newY
        _ ->
            Nothing


{-| Used to get the X and Y values from a position

    ```
    -- A position where x == 3 and y ==2
    positionToXY pos == (3, 2)
    ```
-}
positionToXY : Position -> (Int, Int)
positionToXY { x, y } =
    (x, y)


{-| Used for determining the relative positioning of 2 tiles.
-}
type Neighbor
    = North
    | South
    | East
    | West
    --  NOTE: For future 3D tiling
    -- | Above
    -- | Below


{-| The set of all tiles. Can be easily mapped over for displaying
-}
type alias Board t = Dict String (Tile t)


{-| An empty board.
-}
emptyBoard : Board t
emptyBoard =
    Dict.empty


{-| Dict to List for a Board
-}
boardToList : Board t -> List (Position, Tile t)
boardToList board =
    List.map
        (\(posString, tile) ->
            let
                pos =
                    case positionFromString posString of
                        Nothing -> Position 0 0
                        Just p -> p
            in
                (pos, tile)
        )
        <| Dict.toList board


{-| Used for mapping over a Board. Takes a function and a Board.

The function gets 2 arguments. First a tuple of Ints, in the same format as positionToXY. Second a List of the tile(s).
A single item in the list represents the tile being set to that item. Multiple represents that the tile is still undecided.

    ```
    map (\(x, y) (tile, tiles) -> a) someBoard
    ```
-}
map : ((Int, Int) -> (t, List t) -> a) -> Board t -> List a
map func board =
    List.map
        (\({ x, y }, tile) ->
          func (x, y)
              <| case tile of
                  IsA t ->
                      (t, [])
                  OneOf t ->
                      t
        )
        <| boardToList board


type Tile t
    = IsA t
    | OneOf (t, List t)


{-| Generates a new Board.

Expects an Int width, Int height, function for populating the possible tiles per position in the board, function for validating neighbors, and an initial Seeds.

The function gets 2 arguments. First a tuple of Ints, in the same format as positionToXY. Second a List of the tile(s).
A single item in the list represents the tile being set to that item. Multiple represents that the tile is still undecided.

    ```
    generateBoard width height generateOneOf validateNeighbors seed
    ```

generateOneOf gets a tuple of Ints in the format of positionToXY and returns a tuple of (tile, List tile) representing the possible tiles for this coordinate.

validateNeighbors gets a list of tiles, a list of neighbor tiles, and a Neighbor direction for describing the position of the neighbor tiles relative to the first tiles
-}
generateBoard : Int -> Int -> ((Int, Int) -> (t, List t)) -> (List t -> List t -> Neighbor -> (t, List t)) -> Seed -> (Board t, Seed)
generateBoard width height generateOneOf validateNeighbors seed =
    let
        possibleBoard = generateRow (width - 1) (height - 1) generateOneOf Dict.empty
    in
        populateBoard validateNeighbors seed possibleBoard


generateRow : Int -> Int -> ((Int, Int) -> (t, List t)) -> Board t -> Board t
generateRow x y generateOneOf board =
    let
        nextBoard = generateCell x y generateOneOf board
    in
      if y > 0 then
          generateRow x (y - 1) generateOneOf nextBoard
      else
          nextBoard


generateCell : Int -> Int -> ((Int, Int) -> (t, List t)) -> Board t -> Board t
generateCell x y generateOneOf board =
    let
        pos = { x = x, y = y }
        nextBoard = Dict.insert (positionToString pos) (OneOf (generateOneOf (x, y))) board
    in
        if x > 0 then
            generateCell (x - 1) y generateOneOf nextBoard
        else
            nextBoard


populateBoard : (List t -> List t -> Neighbor -> (t, List t)) -> Seed -> Board t -> (Board t, Seed)
populateBoard validateNeighbors seed board =
    let
        (pickedTile, nextSeed) = pickTile board seed
    in
        case pickedTile of
            Just (posString, OneOf choices) ->
                setTile validateNeighbors nextSeed posString choices board
            _ ->
                (board, nextSeed)


getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing
    else
        List.head <| List.drop idx xs


setTile : (List t -> List t -> Neighbor -> (t, List t)) -> Seed -> String -> (t, List t) -> Board t -> (Board t, Seed)
setTile validateNeighbors seed posString (c, choices) board =
    let
        (selected, nextSeed) = Random.step (Random.uniform c choices) seed
        selectedTile = IsA selected
        nextBoard = Dict.insert posString selectedTile board
        updatedBoard = limitNeighbors validateNeighbors selectedTile posString nextBoard

        (pickedTile, finalSeed) = pickTile updatedBoard nextSeed
    in
        case pickedTile of
            Just (pS, OneOf cs) ->
                setTile validateNeighbors nextSeed pS cs updatedBoard
            _ ->
                (updatedBoard, finalSeed)


pickTile : Board t -> Seed -> (Maybe (String, Tile t), Seed)
pickTile board seed =
    let
        undecidedTiles =
            board
                |> Dict.toList
                |> List.filter needsDeciding
        (i, nextSeed) = Random.step (Random.int 0 <| (List.length undecidedTiles) - 1) seed
        undecidedTile = getAt i undecidedTiles
    in
        (undecidedTile, nextSeed)


needsDeciding : (String, Tile t) -> Bool
needsDeciding (_, tile) =
    case tile of
        IsA _ -> False
        OneOf _ -> True


limitNeighbors : (List t -> List t -> Neighbor -> (t, List t)) -> Tile t -> String -> Board t -> Board t
limitNeighbors validateNeighbors thisTile posString board =
    let
        northBoard =
            case positionFromString posString of
                Nothing ->
                    board
                Just { x, y } ->
                    let
                        pos = positionToString { x = x, y = y + 1 }
                    in
                        case Dict.get pos board of
                            Just (OneOf tiles) ->
                                updateTile validateNeighbors thisTile pos North tiles board
                            _ ->
                                board
        southBoard =
            case positionFromString posString of
                Nothing ->
                    northBoard
                Just { x, y } ->
                    let
                        pos = positionToString { x = x, y = y - 1 }
                    in
                        case Dict.get pos northBoard of
                            Just (OneOf tiles) ->
                                updateTile validateNeighbors thisTile pos South tiles northBoard
                            _ ->
                                northBoard
        eastBoard =
            case positionFromString posString of
                Nothing ->
                    southBoard
                Just { x, y } ->
                    let
                        pos = positionToString { x = x + 1, y = y }
                    in
                        case Dict.get pos southBoard of
                            Just (OneOf tiles) ->
                                updateTile validateNeighbors thisTile pos East tiles southBoard
                            _ ->
                                southBoard
        westBoard =
            case positionFromString posString of
                Nothing ->
                    eastBoard
                Just { x, y } ->
                    let
                        pos = positionToString { x = x - 1, y = y }
                    in
                        case Dict.get pos eastBoard of
                            Just (OneOf tiles) ->
                                updateTile validateNeighbors thisTile pos West tiles eastBoard
                            _ ->
                                eastBoard
    in
        westBoard


updateTile : (List t -> List t -> Neighbor -> (t, List t)) -> Tile t -> String -> Neighbor -> (t, List t) -> Board t -> Board t
updateTile validateNeighbors thisTile pos neighbor (tile, tiles) board =
    let
        possibleThisTiles =
            case thisTile of
                IsA t ->
                    [t]
                OneOf (t, ts) ->
                    t :: ts
        nextTile =
            case validateNeighbors possibleThisTiles (tile :: tiles) neighbor of
                (a, []) ->
                    IsA a
                (a, b) ->
                    OneOf (a, b)
        tileChanges =
            case Dict.get pos board of
                Nothing ->
                    True
                Just currentTile ->
                    currentTile /= nextTile
    in
        if tileChanges then
            let
                changedBoard = Dict.insert pos nextTile board
            in
                limitNeighbors validateNeighbors nextTile pos changedBoard
        else
            board