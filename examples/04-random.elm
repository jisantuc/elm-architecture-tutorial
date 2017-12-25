module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (Svg, svg, circle, rect)
import Svg.Attributes
    exposing
        ( width
        , height
        , fill
        , cx
        , cy
        , r
        )
import Random


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { dieFace1 : Int
    , dieFace2 : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model 1 1, Cmd.none )



-- UPDATE


type Msg
    = Roll
    | NewFace ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace (Random.pair (Random.int 1 6) (Random.int 1 6)) )

        NewFace ( x, y ) ->
            ( Model x y, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


mkCx : Int -> String
mkCx pip =
    if pip > 3 then
        "66"
    else
        "33"


mkCy : Int -> String
mkCy pip =
    case (pip % 3) of
        0 ->
            "20"

        1 ->
            "50"

        _ ->
            "80"


mkCircle : Int -> Svg Msg
mkCircle pip =
    circle [ cx (mkCx pip), cy (mkCy pip), r "5", fill "red" ] []


addPip : Int -> List (Svg Msg) -> List (Svg Msg)
addPip x pips =
    pips ++ [ mkCircle x ]


mkSvg : Int -> Svg Msg
mkSvg dieVal =
    svg [ width "100", height "100" ] (List.foldl addPip [] (List.range 1 dieVal))


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ mkSvg model.dieFace1
            , mkSvg model.dieFace2
            ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]
