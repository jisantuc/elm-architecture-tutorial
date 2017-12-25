module Main exposing (..)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Helpers


type alias StrokeWidth =
    Int



-- for rotating the clock to a human-oriented angle


halfRadian : Float
halfRadian =
    pi / 2


mkRadialLine : Float -> Float -> StrokeWidth -> Svg Msg
mkRadialLine x y s =
    line [ x1 "50", y1 "50", x2 (toString x), y2 (toString y), stroke "#023963", strokeWidth (toString s) ] []



-- convert an angle and ray length to (x, y) coordinates


radiansToXY : Float -> Float -> ( Float, Float )
radiansToXY rad len =
    ( 50 + len * cos rad, 50 + len * sin rad )



-- MODEL


type alias Model =
    { hours : Time
    , minutes : Time
    , seconds : Time
    , paused : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 0 0 False, Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                hours =
                    Time.inHours newTime

                minutes =
                    Time.inMinutes newTime

                seconds =
                    newTime
            in
                ( Model hours minutes seconds False, Cmd.none )

        Pause ->
            ( { model | paused = not model.paused }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.paused then
        Time.every second Tick
    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        secondsAngle =
            turns (Time.inMinutes model.seconds) - halfRadian

        minutesAngle =
            turns (toFloat ((floor model.minutes) % 60) / 60) - halfRadian

        hoursAngle =
            turns (toFloat ((floor model.hours) % 12) / 12) - halfRadian

        (secondHandX, secondHandY) = radiansToXY secondsAngle 40
        (minuteHandX, minuteHandY) = radiansToXY minutesAngle 40
        (hourHandX, hourHandY) = radiansToXY hoursAngle 25

    in
        Html.div []
            [ Html.button [ onClick Pause ]
                [ Html.text
                    (if not model.paused then
                        "FREEZE"
                     else
                        "UNFREEZE"
                    )
                ]
            , Html.br [] []
            , svg [ viewBox "0 0 100 100", width "300px" ]
                [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
                , mkRadialLine secondHandX secondHandY 1
                , mkRadialLine minuteHandX minuteHandY 2
                , mkRadialLine hourHandX hourHandY 2
                ]
            ]
