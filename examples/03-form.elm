module Main exposing (..)

import String exposing (all)
import Char exposing (isDigit)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    , showErrors : Bool
    }


model : Model
model =
    Model "" "" "" "" False



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name, showErrors = False }

        Password password ->
            { model | password = password, showErrors = False }

        PasswordAgain password ->
            { model | passwordAgain = password, showErrors = False }

        Age age ->
            { model | age = age, showErrors = False }

        Submit ->
            { model | showErrors = True }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Name", onInput Name ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
        , input [ type_ "text", placeholder "Age", onInput Age ] []
        , button [ onClick Submit ] [ text "Submit" ]
        , viewValidation model
        ]


viewValidation : Model -> Html msg
viewValidation model =
    let
        passwordsMatch =
            model.password == model.passwordAgain

        passwordLengthOk =
            String.length model.password >= 8

        ageIsNum =
            all isDigit model.age

        ( color, message ) =
            case ( passwordsMatch, passwordLengthOk, ageIsNum ) of
                ( True, True, True ) ->
                    ( "green", "OK" )

                ( True, True, False ) ->
                    ( "red", "Age isn't a number" )

                ( True, False, _ ) ->
                    ( "red", "Password too short" )

                ( False, True, _ ) ->
                    ( "red", "Passwords don't match" )

                ( False, False, _ ) ->
                    ( "red", "come on dude" )
    in
        if model.showErrors then
            div [ style [ ( "color", color ) ] ] [ text message ]
        else
            div [] []
