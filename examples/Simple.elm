module Simple exposing (main)


import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrameDelta)
import Debug exposing (log)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Random exposing (Generator, Seed)
import Task
import Time exposing (Posix)
import Tiler exposing (Board, Neighbor(..))


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


---- TYPES ----


type alias Model =
    { randomSeed : Seed
    , board : Board Cell
    }


type Msg
    = NoOp
    | InitializeRandomness Posix
    | Tick Float
    


type alias Cell =
    { style : CellStyle
    , rotation : Rotation
    }


cellSize : Float
cellSize =
    10


boardWidth : Int
boardWidth =
    6


boardHeight : Int
boardHeight =
    6


-- Counter clock wise
type Rotation
    = RNone
    | RQuarter
    | RHalf
    | RThreeQuarters


type CellStyle
    = Hallway --  North <-> South hallway
    | Junction -- 4 way intersection
    | Corner -- West <-> South hallway
    | Tee -- West <-> North <-> South intersection
    | DeadEnd -- Enter from South


---- INIT ----


init : () -> (Model, Cmd Msg)
init flags =
    ( { randomSeed = Random.initialSeed 0
      , board = Tiler.emptyBoard
      }
    , Cmd.batch
          [ Task.perform InitializeRandomness Time.now
          ]
    )


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [
        -- [ onAnimationFrameDelta Tick
        ]


---- UPDATE ----


update : Msg -> Model -> (Model, Cmd Msg)
update msg model=
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Tick _ ->
            ( model, Cmd.none )
        InitializeRandomness now ->
            let
                -- seed = Random.initialSeed 1543524430781
                seed = Random.initialSeed <| log "initial seed" <| Time.posixToMillis now
                (board, nextSeed) = Tiler.generateBoard boardWidth boardHeight generateOneOf validateNeighbors seed
            in
                ( { model
                  | randomSeed = nextSeed
                  , board = board
                  }
                , Cmd.none
                )


generateOneOf : (Int, Int) -> (Cell, List Cell)
generateOneOf _ =
    ( Cell Hallway RNone
    , [ Cell Hallway RQuarter
      , Cell Hallway RNone
      , Cell Hallway RQuarter
      , Cell Corner RNone
      , Cell Corner RQuarter
      , Cell Corner RHalf
      , Cell Corner RThreeQuarters
      , Cell DeadEnd RNone
      , Cell DeadEnd RQuarter
      , Cell DeadEnd RHalf
      , Cell DeadEnd RThreeQuarters
      , Cell Junction RNone
      , Cell Tee RNone
      , Cell Tee RQuarter
      , Cell Tee RHalf
      , Cell Tee RThreeQuarters
      ]
    )


validateNeighbors : List Cell -> List Cell -> Neighbor -> (Cell, List Cell)
validateNeighbors possibleThisTiles possibleNeighbors neighborDirection =
    let
        filteredPossibilities =
            List.filter (\neigh -> List.any (\self -> validJunction neighborDirection self neigh) possibleThisTiles) possibleNeighbors
    in
        case filteredPossibilities of
            (t :: others) ->
                (t, others)
            [] ->
                -- TODO: Find a way to never reach this
                (Cell Hallway RNone, [])


validJunction : Neighbor -> Cell -> Cell -> Bool
validJunction neighborDirection self neighbor =
    case (self.style, self.rotation, neighborDirection) of
        -- Hallway
        -- Hallway, North neighbor
        (Hallway, RNone, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                _ -> False
        (Hallway, RHalf, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                _ -> False
        (Hallway, RQuarter, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Hallway, RThreeQuarters, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        -- Hallway, South neighbor
        (Hallway, RNone, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Hallway, RHalf, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Hallway, RQuarter, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RQuarter) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Hallway, RThreeQuarters, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RQuarter) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        -- Hallway, East neighbor
        (Hallway, RNone, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Hallway, RHalf, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Hallway, RQuarter, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Hallway, RThreeQuarters, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        -- Hallway, West neighbor
        (Hallway, RNone, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Hallway, RHalf, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Hallway, RQuarter, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                _ -> False
        (Hallway, RThreeQuarters, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                _ -> False

        -- Corner
        -- Corner, North neighbor
        (Corner, RNone, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Corner, RQuarter, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Corner, RHalf, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                _ -> False
        (Corner, RThreeQuarters, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                _ -> False
        -- Corner, South neighbor
        (Corner, RNone, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Corner, RQuarter, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Corner, RHalf, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RQuarter) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Corner, RThreeQuarters, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RQuarter) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        -- Corner, East neighbor
        (Corner, RNone, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Corner, RQuarter, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Corner, RHalf, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Corner, RThreeQuarters, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        -- Corner, West neighbor
        (Corner, RNone, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                _ -> False
        (Corner, RQuarter, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Corner, RHalf, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Corner, RThreeQuarters, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                _ -> False

        -- Dead End
        -- Dead End, North neighbor
        (DeadEnd, RNone, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (DeadEnd, RQuarter, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (DeadEnd, RHalf, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                _ -> False
        (DeadEnd, RThreeQuarters, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        -- Dead End, South neighbor
        (DeadEnd, RNone, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (DeadEnd, RQuarter, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RQuarter) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (DeadEnd, RHalf, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RQuarter) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (DeadEnd, RThreeQuarters, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RQuarter) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        -- Dead End, East neighbor
        (DeadEnd, RNone, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (DeadEnd, RQuarter, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (DeadEnd, RHalf, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (DeadEnd, RThreeQuarters, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        -- Dead End, West neighbor
        (DeadEnd, RNone, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (DeadEnd, RQuarter, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (DeadEnd, RHalf, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (DeadEnd, RThreeQuarters, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                _ -> False

        -- Junction
        -- Junction, North neighbor
        (Junction, RNone, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                _ -> False
        (Junction, RQuarter, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                _ -> False
        (Junction, RHalf, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                _ -> False
        (Junction, RThreeQuarters, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                _ -> False
        -- Junction, South neighbor
        (Junction, RNone, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Junction, RQuarter, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Junction, RHalf, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Junction, RThreeQuarters, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        -- Junction, East neighbor
        (Junction, RNone, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Junction, RQuarter, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Junction, RHalf, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Junction, RThreeQuarters, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        -- Junction, West neighbor
        (Junction, RNone, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                _ -> False
        (Junction, RQuarter, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                _ -> False
        (Junction, RHalf, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                _ -> False
        (Junction, RThreeQuarters, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                _ -> False

        -- Tee
        -- Tee, North neighbor
        (Tee, RNone, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                _ -> False
        (Tee, RQuarter, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Tee, RHalf, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                _ -> False
        (Tee, RThreeQuarters, North) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                _ -> False
        -- Tee, South neighbor
        (Tee, RNone, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Tee, RQuarter, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Tee, RHalf, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Junction, _) -> True
                (Corner, RHalf) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Tee, RThreeQuarters, South) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Corner, RNone) -> True
                (Corner, RQuarter) -> True
                (Tee, RQuarter) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        -- Tee, East neighbor
        (Tee, RNone, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RHalf) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RQuarter) -> True
                (DeadEnd, RHalf) -> True
                _ -> False
        (Tee, RQuarter, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Tee, RHalf, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Tee, RThreeQuarters, East) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (Tee, RQuarter) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        -- Tee, West neighbor
        (Tee, RNone, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                _ -> False
        (Tee, RQuarter, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                _ -> False
        (Tee, RHalf, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RNone) -> True
                (Hallway, RHalf) -> True
                (Corner, RNone) -> True
                (Corner, RThreeQuarters) -> True
                (Tee, RNone) -> True
                (DeadEnd, RNone) -> True
                (DeadEnd, RHalf) -> True
                (DeadEnd, RThreeQuarters) -> True
                _ -> False
        (Tee, RThreeQuarters, West) ->
            case (neighbor.style, neighbor.rotation) of
                (Hallway, RQuarter) -> True
                (Hallway, RThreeQuarters) -> True
                (Junction, _) -> True
                (Corner, RQuarter) -> True
                (Corner, RHalf) -> True
                (Tee, RQuarter) -> True
                (Tee, RHalf) -> True
                (Tee, RThreeQuarters) -> True
                (DeadEnd, RQuarter) -> True
                _ -> False


---- VIEW ----


view : Model -> Document Msg
view { board } =
    { title = "Maze"
    , body =
        [ Html.div
              [ Attrs.style "font-family" "sans-serif" ]
              <| Tiler.map drawTile board
        ]
    }


drawTile : (Int, Int) -> (Cell, List Cell) -> Html Msg
drawTile pos (tile, tiles) =
    case tiles of
        [] ->
            drawCell pos tile
        _ ->
            drawUndecided pos (String.join "\n" (List.map (\{ style } -> cellStyleToString style) (tile :: tiles)))


drawCell : (Int, Int) -> Cell -> Html Msg
drawCell (x, y) { style, rotation } =
    let
        rotInDeg =
            case rotation of
                RNone -> "0"
                RQuarter -> "-90"
                RHalf -> "-180"
                RThreeQuarters -> "-270"
    in
        Html.div
            [ Attrs.style "position" "absolute"
            , Attrs.style "width"
                  <| (String.fromFloat cellSize) ++ "rem"
            , Attrs.style "height"
                  <| (String.fromFloat cellSize) ++ "rem"
            , Attrs.style "left"
                  <| (String.fromFloat <| (toFloat x) * cellSize) ++ "rem"
            , Attrs.style "bottom"
                  <| (String.fromFloat <| (toFloat y) * cellSize) ++ "rem"
            , Attrs.style "transform"
                  <| "rotate(" ++ rotInDeg ++ "deg)"
            -- , Attrs.style "outline" "1px solid black"
            ]
            [ case style of
                  Hallway ->
                      drawHallway
                  Corner ->
                      drawCorner
                  DeadEnd ->
                      drawDeadEnd
                  Junction ->
                      drawJunction
                  Tee ->
                      drawTee
            ]


drawHallway : Html Msg
drawHallway =
    Html.div
        [ Attrs.style "position" "relative"
        , Attrs.style "width" "50%"
        , Attrs.style "height" "100%"
        , Attrs.style "left" "25%"
        , Attrs.style "background-color" "black"
        ]
        []


drawCorner : Html Msg
drawCorner =
    Html.div
        [ Attrs.style "width" "100%"
        , Attrs.style "height" "100%"
        ]
        [ Html.div
              [ Attrs.style "position" "relative"
              , Attrs.style "width" "75%"
              , Attrs.style "height" "50%"
              , Attrs.style "left" "0"
              , Attrs.style "top" "25%"
              , Attrs.style "background-color" "black"
              ]
              []
        , Html.div
              [ Attrs.style "position" "relative"
              , Attrs.style "width" "50%"
              , Attrs.style "height" "75%"
              , Attrs.style "left" "25%"
              , Attrs.style "top" "-25%"
              , Attrs.style "background-color" "black"
              ]
              []
        ]


drawJunction : Html Msg
drawJunction =
    Html.div
        [ Attrs.style "width" "100%"
        , Attrs.style "height" "100%"
        ]
        [ Html.div
              [ Attrs.style "position" "relative"
              , Attrs.style "width" "50%"
              , Attrs.style "height" "100%"
              , Attrs.style "left" "25%"
              , Attrs.style "background-color" "black"
              ]
              []
        , Html.div
              [ Attrs.style "position" "relative"
              , Attrs.style "width" "100%"
              , Attrs.style "height" "50%"
              , Attrs.style "top" "-75%"
              , Attrs.style "background-color" "black"
              ]
              []
        ]


drawTee : Html Msg
drawTee =
    Html.div
        [ Attrs.style "width" "100%"
        , Attrs.style "height" "100%"
        ]
        [ Html.div
              [ Attrs.style "position" "relative"
              , Attrs.style "width" "50%"
              , Attrs.style "height" "100%"
              , Attrs.style "left" "25%"
              , Attrs.style "background-color" "black"
              ]
              []
        , Html.div
              [ Attrs.style "position" "relative"
              , Attrs.style "width" "75%"
              , Attrs.style "height" "50%"
              , Attrs.style "top" "-75%"
              , Attrs.style "background-color" "black"
              ]
              []
        ]


drawDeadEnd : Html Msg
drawDeadEnd =
    Html.div
        [ Attrs.style "position" "relative"
        , Attrs.style "width" "50%"
        , Attrs.style "height" "75%"
        , Attrs.style "left" "25%"
        , Attrs.style "top" "25%"
        , Attrs.style "background-color" "black"
        ]
        []


drawUndecided : (Int, Int) -> String -> Html Msg
drawUndecided (x, y) style =
    Html.div
        [ Attrs.style "position" "absolute"
        , Attrs.style "width" <| (String.fromFloat cellSize) ++ "rem"
        , Attrs.style "height" <| (String.fromFloat cellSize) ++ "rem"
        , Attrs.style "left" <| (String.fromFloat <| (toFloat x) * cellSize) ++ "rem"
        , Attrs.style "bottom" <| (String.fromFloat <| (toFloat y) * cellSize) ++ "rem"
        , Attrs.style "white-space" "pre-wrap"
        ]
        [ Html.text style
        , Html.br [] []
        , Html.text <| (String.fromInt x) ++ ", " ++ (String.fromInt y)
        ]


cellStyleToString : CellStyle -> String
cellStyleToString style =
    case style of
        Hallway -> "Hallway"
        Junction -> "Junction"
        Tee -> "Tee"
        Corner -> "Corner"
        DeadEnd -> "Dead End"