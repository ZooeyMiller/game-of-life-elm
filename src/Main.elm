module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Array exposing (..)


---- MODEL ----


type alias Model =
    { game : Grid
    , running : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model (createInitialGrid 100) False, Cmd.none )



---- UPDATE ----


type Msg
    = ToggleRunning
    | Cycle
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        Cycle ->
            ( { model | game = newGeneration model.game }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


type Cell
    = Alive
    | Dead


type alias Grid =
    Array (Array Cell)


type alias Coordinate =
    ( Int, Int )


newGeneration : Grid -> Grid
newGeneration grid =
    Array.indexedMap (\y array -> Array.indexedMap (\x cell -> liveOrDie cell <| getTotalLiveNeighbors <| findNeigbours x y grid) array) grid


createInitialGrid : Int -> Grid
createInitialGrid h =
    Array.initialize h <| always (Array.initialize h (always Alive))


getInGrid : Grid -> Coordinate -> Maybe Cell
getInGrid grid ( x, y ) =
    Maybe.andThen (Array.get y) <| Array.get x grid


setInGrid : Coordinate -> Grid -> Cell -> Maybe Grid
setInGrid ( x, y ) grid value =
    Maybe.map (\v -> Array.set y v grid) <| Maybe.map (Array.set x value) <| Array.get y grid


findNeigbours : Int -> Int -> Grid -> List (Maybe Cell)
findNeigbours x y grid =
    List.map (getInGrid grid) <| getNeigbourCoordinates x y


getTotalLiveNeighbors : List (Maybe Cell) -> Int
getTotalLiveNeighbors list =
    List.foldr (handleMaybeCellFold) 0 list


handleMaybeCellFold : Maybe Cell -> Int -> Int
handleMaybeCellFold maybeCell total =
    case maybeCell of
        Just cell ->
            case cell of
                Alive ->
                    total + 1

                _ ->
                    total

        Nothing ->
            total


liveOrDie : Cell -> Int -> Cell
liveOrDie cell livingNeighbours =
    case cell of
        Alive ->
            if livingNeighbours < 2 then
                Dead
            else if livingNeighbours > 3 then
                Dead
            else
                Alive

        Dead ->
            if livingNeighbours == 3 then
                Alive
            else
                Dead


getNeigbourCoordinates : Int -> Int -> List Coordinate
getNeigbourCoordinates x y =
    [ ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    , ( x - 1, y + 1 )
    , ( x, y + 1 )
    , ( x + 1, y + 1 )
    ]
