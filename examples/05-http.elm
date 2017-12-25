module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


main =
    Html.program
        { init = init "cats"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { topic : String
    , gifUrl : String
    , error : String
    }


init : String -> ( Model, Cmd Msg )
init topic =
    ( Model topic "waiting.gif" ""
    , getRandomGif topic
    )



-- UPDATE


type Msg
    = MorePlease
    | Topic String
    | NewGif (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getRandomGif model.topic )

        Topic s ->
            ( { model | topic = s }, Cmd.none )

        NewGif (Ok newUrl) ->
            ( Model model.topic newUrl "", Cmd.none )

        NewGif (Err e) ->
            ( { model | error = "Something went wrong" }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Fresh gifs for you" ]
        , div [ style [ ( "color", "red" ) ] ] [ text model.error ]
        , span [ style [ ( "padding", "2px" ) ] ] [ text "Choose gif topic somehow" ]
        , br [] []
        , select [ style [ ( "padding", "2px" ) ], onInput Topic ]
            [ option [] [ text "cats" ]
            , option [] [ text "pandas" ]
            , option [] [ text "emus" ]
            ]
        , br [] []
        , input [ style [ ( "padding", "2px" ) ], type_ "text", placeholder "Topic", onInput Topic ] []
        , br [] []
        , button [ style [ ( "padding", "2px" ) ], onClick MorePlease ] [ text "get some gifs!" ]
        , br [] []
        , img [ src model.gifUrl ] []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    in
        Http.send NewGif (Http.get url decodeGifUrl)


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
    Decode.at [ "data", "image_url" ] Decode.string
