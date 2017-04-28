module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)
import Number.Bounded as Bounded exposing (Bounded)


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
    ( Model
        (BoundedPosition
            (Bounded.between 0 600 |> Bounded.set 200)
            (Bounded.between 0 600 |> Bounded.set 200)
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


(=>) : a -> a -> ( a, a )
(=>) =
    (,)


view : Model -> Html Msg
view model =
    let
        realPosition =
            getPosition model
                |> toPosition
    in
        div [ class "board" ]
            [ div
                [ onMouseDown
                , style
                    [ "background-color" => "#3C8D2F"
                    , "cursor" => "move"
                    , "width" => "50px"
                    , "height" => "50px"
                    , "border-radius" => "4px"
                    , "position" => "absolute"
                    , "left" => px realPosition.x
                    , "top" => px realPosition.y
                    , "color" => "white"
                    , "display" => "flex"
                    , "align-items" => "center"
                    , "justify-content" => "center"
                    ]
                ]
                [ text "Drag Me!"
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
