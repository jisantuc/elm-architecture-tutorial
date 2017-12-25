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

-- for rotating the clock to a human-oriented angle
halfRadian : Float
halfRadian = pi / 2

view : Model -> Html Msg
view model =
    let
        secondsAngle =
            turns (Time.inMinutes model.seconds) - halfRadian

        secondHandX =
            toString (50 + 40 * cos secondsAngle)

        secondHandY =
            toString (50 + 40 * sin secondsAngle)

        minutesAngle =
            turns (toFloat ((floor model.minutes) % 60) / 60) - halfRadian

        minuteHandX =
            toString (50 + 40 * cos minutesAngle)

        minuteHandY =
            toString (50 + 40 * sin minutesAngle)

        hoursAngle =
            turns (toFloat ((floor model.hours) % 12) / 12) - halfRadian

        hourHandX =
            toString (50 + 25 * cos hoursAngle)

        hourHandY =
            toString (50 + 25 * sin hoursAngle)
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
                , line [ x1 "50", y1 "50", x2 secondHandX, y2 secondHandY, stroke "#023963", strokeWidth "1" ] []
                , line [ x1 "50", y1 "50", x2 minuteHandX, y2 minuteHandY, stroke "#023963", strokeWidth "2" ] []
                , line [ x1 "50", y1 "50", x2 hourHandX, y2 hourHandY, stroke "#023963", strokeWidth "2" ] []
                ]
            ]
