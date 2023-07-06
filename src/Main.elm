port module Main exposing (Flags, Model, Msg, main)

import Audio
import Browser
import Effect
import Game
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Encode
import WebAudio
import WebAudio.Property


port toWebAudio : Json.Encode.Value -> Cmd msg


type Model
    = WaitingWebAudioInit
    | WebAudioReady InnerModel


type alias InnerModel =
    { freq : Float
    , delay : Float
    , game : Game.Model
    }


type Msg
    = GameMsg Game.Msg
    | InitWebAudio


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


audioCmd : InnerModel -> Cmd Msg
audioCmd model =
    audio model
        |> Json.Encode.list WebAudio.encode
        |> toWebAudio


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( WaitingWebAudioInit
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GameMsg gameMsg, WebAudioReady innerModel ) ->
            let
                ( newGame, gameEffect ) =
                    Game.update gameMsg innerModel.game
            in
            ( WebAudioReady
                { innerModel
                    | game = newGame
                }
            , Cmd.batch
                [ Effect.toCmd gameEffect
                , audioCmd innerModel
                ]
            )

        ( InitWebAudio, WaitingWebAudioInit ) ->
            let
                ( game, gameEffect ) =
                    Game.init

                innerModel : InnerModel
                innerModel =
                    { freq = 220
                    , delay = 0.01
                    , game = game
                    }
            in
            ( WebAudioReady innerModel
            , Cmd.batch
                [ Effect.toCmd gameEffect
                , audioCmd innerModel
                ]
            )

        ( GameMsg _, WaitingWebAudioInit ) ->
            ( model, Cmd.none )

        ( InitWebAudio, WebAudioReady _ ) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        WaitingWebAudioInit ->
            Html.div
                [ Html.Events.onClick InitWebAudio
                , Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "top" "0"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "width" "100vw"
                , Html.Attributes.style "height" "100vh"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "justify-content" "center"
                ]
                [ Html.div [] [ Html.text "Click anywhere to start" ]
                ]

        WebAudioReady innerModel ->
            Html.map GameMsg <| Game.view innerModel.game


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        WebAudioReady innerModel ->
            Sub.map GameMsg (Game.subscriptions innerModel.game)

        WaitingWebAudioInit ->
            Sub.none


audio : InnerModel -> List WebAudio.Node
audio model =
    [ WebAudio.oscillator
        [ WebAudio.Property.frequency model.freq
        ]
        |> Audio.gain 0.2
        |> Audio.destination
    ]
