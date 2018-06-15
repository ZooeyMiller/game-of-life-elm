module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Time exposing (..)


---- MODEL ----


type alias Model =
    { game : Grid
    , running : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model (createInitialGrid 20) False, Cmd.none )



---- UPDATE ----


type Msg
    = ToggleRunning
    | Cycle
    | ToggleCell Int Int Cell
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        Cycle ->
            ( { model | game = newGeneration model.game }, Cmd.none )

        ToggleCell x y cell ->
            ( { model | game = Maybe.withDefault model.game (setInGrid ( x, y ) model.game (toggleAlive cell)) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        (((::)
            (renderGrid model.game)
            ([ [ Html.button [ Html.Events.onClick ToggleRunning ]
                    [ text
                        (if model.running then
                            "stop"
                         else
                            "start"
                        )
                    ]
               ]
             ]
            )
         )
            |> List.concat
        )


renderGrid : Array (Array Cell) -> List (Html Msg)
renderGrid grid =
    Array.indexedMap
        (\y a -> (Array.indexedMap (\x cell -> div [ Html.Events.onClick (ToggleCell x y cell), Html.Attributes.style [ ( "background-color", colour cell ), ( "height", "10px" ), ( "width", "10px" ), ( "display", "inline-block" ), ( "margin", "1px" ), ( "border", "1px solid black" ) ] ] []) a |> Array.toList) :: [ [ Html.br [] [] ] ])
        grid
        |> Array.toList
        |> List.concat
        |> List.concat


colour : Cell -> String
colour cell =
    case cell of
        Alive ->
            "black"

        _ ->
            "white"



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }



--------SUBSCRIPTIONS--------


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every (500 * Time.millisecond) (always Cycle)
    else
        Sub.none


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
    Array.initialize h <| always (Array.initialize h (always Dead))


getInGrid : Grid -> Coordinate -> Maybe Cell
getInGrid grid ( x, y ) =
    Maybe.andThen (Array.get x) <| Array.get y grid


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


toggleAlive : Cell -> Cell
toggleAlive cell =
    case cell of
        Alive ->
            Dead

        _ ->
            Alive
