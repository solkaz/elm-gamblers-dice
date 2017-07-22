module Main exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import GamblersDice
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { faces : Int
    , gamblerState : Array Int
    , previousGamblerResult : Maybe Int
    , gamblerResults : List Int
    }


initialModel : Model
initialModel =
    { faces = 4
    , gamblerState = Array.repeat 4 1
    , previousGamblerResult = Nothing
    , gamblerResults = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = IncrementDieFace
    | DecrementDieFace
    | RollGamblerDie
    | AddGamblerResult Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementDieFace ->
            ( { model
                | faces = model.faces + 1
                , gamblerState = Array.repeat (model.faces + 1) 1
                , gamblerResults = []
              }
            , Cmd.none
            )

        DecrementDieFace ->
            if model.faces > 2 then
                ( { model
                    | faces = model.faces - 1
                    , gamblerState = Array.repeat (model.faces - 1) 1
                    , gamblerResults = []
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        RollGamblerDie ->
            let
                sum =
                    GamblersDice.sum model.gamblerState
            in
                ( model, GamblersDice.generateSteps sum AddGamblerResult )

        AddGamblerResult steps ->
            let
                result =
                    GamblersDice.roll model.gamblerState steps

                newState =
                    Tuple.first result

                dieResult =
                    Tuple.second result
            in
                ( { model
                    | gamblerState = newState
                    , previousGamblerResult = Maybe.Just dieResult
                    , gamblerResults = (dieResult :: model.gamblerResults)
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        resultDisplay =
            Maybe.withDefault 0 model.previousGamblerResult
                |> toString
                |> ((++) "Result: ")
                |> text
    in
        div []
            [ h1 [] [ text "Gambler's dice" ]
            , button [ onClick DecrementDieFace ] [ text "-" ]
            , div [] [ text <| toString model.faces ]
            , button [ onClick IncrementDieFace ] [ text "+" ]
            , br [] []
            , button [ onClick RollGamblerDie ] [ text "Roll" ]
            , br [] []
            , h3 [] [ resultDisplay ]
            , ul []
                (List.map
                    (\result -> li [] [ text <| toString result ])
                    (List.reverse model.gamblerResults)
                )
            ]
