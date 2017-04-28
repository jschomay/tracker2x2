module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)
import Number.Bounded as Bounded exposing (Bounded)
import Http


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


type alias Story =
    { position : BoundedPosition
    , title : String
    , id : Int
    }


type alias Model =
    { stories : List Story
    , drag : Maybe Drag
    }


type alias Drag =
    { id : Int
    , start : Position
    , current : Position
    }


type alias RequestParams =
    { projectId : String
    , label : String
    , token : String
    }


requestParams : RequestParams
requestParams =
    { projectId = "111111"
    , label = "Some label"
    , token = "my Token"
    }


init : ( Model, Cmd Msg )
init =
    let
        middleX =
            (board.width - item.width) // 2

        middleY =
            (board.height - item.height) // 2

        setPosition position =
            Bounded.between 0 (board.width - item.width)
                |> Bounded.set position

        stories =
            []
    in
        ( { stories = stories
          , drag = Nothing
          }
        , Http.send StoriesResponse (getStories requestParams)
        )


storyDecoder : Decode.Decoder Story
storyDecoder =
    let
        middleX =
            (board.width - item.width) // 2

        middleY =
            (board.height - item.height) // 2

        setPosition position itemSize =
            Bounded.between 0 (board.width - itemSize)
                |> Bounded.set position
    in
        Decode.map2
            (Story
                { x = setPosition middleX item.width
                , y = setPosition middleY item.height
                }
            )
            (Decode.field "name" Decode.string)
            (Decode.field "id" Decode.int)



-- Decode.succeed <|
--     { position =
--         { x = setPosition middleX
--         , y = setPosition middleY
--         }
--     , title = "A story"
--     , id = 2
--     }


getStories : RequestParams -> Http.Request (List Story)
getStories { projectId, label, token } =
    let
        storiesUrl =
            "https://www.pivotaltracker.com/services/v5/projects/" ++ projectId ++ "/stories?with_label=" ++ label

        storiesDecoder =
            (Decode.list storyDecoder)
    in
        Http.request
            { method = "GET"
            , headers =
                [ Http.header "Content-Type" "application/json"
                , Http.header "X-TrackerToken" token
                ]
            , url = storiesUrl
            , body = Http.emptyBody
            , expect = Http.expectJson storiesDecoder
            , timeout = Nothing
            , withCredentials = False
            }


toPosition : BoundedPosition -> Position
toPosition bp =
    Position (Bounded.value bp.x) (Bounded.value bp.y)



-- UPDATE


type Msg
    = DragStart Int Position
    | DragAt Position
    | DragEnd Position
    | StoriesResponse (Result Http.Error (List Story))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart _ _ ->
            ( { model | drag = updateDrag msg model.drag }
            , Cmd.none
            )

        DragAt _ ->
            ( { model | drag = updateDrag msg model.drag }
            , Cmd.none
            )

        DragEnd _ ->
            case model.drag of
                Nothing ->
                    ( model, Cmd.none )

                Just drag ->
                    ( { model
                        | stories = List.map (updateStoryPosition model.drag) model.stories
                        , drag = Nothing
                      }
                    , Cmd.none
                    )

        StoriesResponse result ->
            case result of
                Err error ->
                    Debug.crash "we suck"

                Ok stories ->
                    ( { model | stories = stories }, Cmd.none )


updateStoryPosition : Maybe Drag -> Story -> Story
updateStoryPosition maybeDrag story =
    case maybeDrag of
        Just drag ->
            if drag.id == story.id then
                { story | position = getPosition maybeDrag story }
            else
                story

        _ ->
            story


updateDrag : Msg -> Maybe Drag -> Maybe Drag
updateDrag msg drag =
    case msg of
        DragStart id xy ->
            Just
                { id = id
                , start = xy
                , current = xy
                }

        DragAt xy ->
            Maybe.map
                (\drag ->
                    { drag
                        | start = drag.start
                        , current = xy
                    }
                )
                drag

        _ ->
            drag



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
    div
        [ class "board"
        , style
            [ ( "width", px board.width )
            , ( "height", px board.height )
            ]
        ]
    <|
        [ div [ class "board__axis board__axis--y" ] []
        , div [ class "board__axis board__axis--x" ] []
        ]
            ++ List.map (itemView model.drag) model.stories


itemView : Maybe Drag -> Story -> Html Msg
itemView drag story =
    let
        realPosition =
            getPosition drag story
                |> toPosition
    in
        div
            [ class "item"
            , onMouseDown story.id
            , style
                [ ( "left", px realPosition.x )
                , ( "top", px realPosition.y )
                , ( "width", px item.width )
                , ( "height", px item.height )
                ]
            ]
            [ span [ class "item--title" ] [ text <| story.title ]
            ]


px : Int -> String
px number =
    toString number ++ "px"


getPosition : Maybe Drag -> Story -> BoundedPosition
getPosition drag story =
    case drag of
        Nothing ->
            story.position

        Just { id, start, current } ->
            if id == story.id then
                BoundedPosition
                    (Bounded.set ((Bounded.value story.position.x) + current.x - start.x) story.position.x)
                    (Bounded.set ((Bounded.value story.position.y) + current.y - start.y) story.position.y)
            else
                story.position


onMouseDown : Int -> Attribute Msg
onMouseDown id =
    on "mousedown" (Decode.map (DragStart id) Mouse.position)
