module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)
import Number.Bounded as Bounded exposing (Bounded)


type alias Size =
    { width : Int
    , height : Int
    }


board : Size
board =
    { width = 650, height = 650 }


item : Size
item =
    { width = 90, height = 30 }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias BoundedPosition =
    { x : Bounded Int
    , y : Bounded Int
    }


type alias Model =
    { position : BoundedPosition
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init =
    let
        middleX =
            (board.width - item.width) // 2

        middleY =
            (board.height - item.height) // 2
    in
        ( Model
            (BoundedPosition
                (Bounded.between 0 (board.width - item.width) |> Bounded.set middleX)
                (Bounded.between 0 (board.height - item.height) |> Bounded.set middleY)
            )
            Nothing
        , Cmd.none
        )


toPosition : BoundedPosition -> Position
toPosition bp =
    Position (Bounded.value bp.x) (Bounded.value bp.y)



-- UPDATE


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({ position, drag } as model) =
    case msg of
        DragStart xy ->
            Model position (Just (Drag xy xy))

        DragAt xy ->
            Model position (Maybe.map (\{ start } -> Drag start xy) drag)

        DragEnd _ ->
            Model (getPosition model) Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        realPosition =
            getPosition model
                |> toPosition
    in
        div
            [ class "board"
            , style
                [ ( "width", px board.width )
                , ( "height", px board.height )
                ]
            ]
            [ div [ class "board__axis board__axis--y" ] []
            , div [ class "board__axis board__axis--x" ] []
            , div
                [ class "item"
                , onMouseDown
                , style
                    [ ( "left", px realPosition.x )
                    , ( "top", px realPosition.y )
                    , ( "width", px item.width )
                    , ( "height", px item.height )
                    ]
                ]
                [ span [ class "item--title" ] [ text <| "User can drag stories around on a 2x2." ]
                ]
            ]


px : Int -> String
px number =
    toString number ++ "px"


getPosition : Model -> BoundedPosition
getPosition { position, drag } =
    case drag of
        Nothing ->
            position

        Just { start, current } ->
            BoundedPosition
                (Bounded.set ((Bounded.value position.x) + current.x - start.x) position.x)
                (Bounded.set ((Bounded.value position.y) + current.y - start.y) position.y)


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map DragStart Mouse.position)
