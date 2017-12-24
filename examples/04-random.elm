import Html exposing (..)
import Html.Attributes exposing (src, style)
import Html.Events exposing (..)
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


init : (Model, Cmd Msg)
init =
  (Model 1 1, Cmd.none)



-- UPDATE


type Msg
  = Roll
  | NewFace (Int, Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFace (Random.pair (Random.int 1 6) (Random.int 1 6)))

    NewFace (x, y) ->
      (Model x y, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


mkImage : Int -> Html Msg
mkImage dieFace =
    img [src ("images/dice-" ++ toString dieFace ++ ".png")
        , style [("width", "25%")]] []

view : Model -> Html Msg
view model =
  div []
    [div [] [ mkImage model.dieFace1
            ,  mkImage model.dieFace2
            ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]
