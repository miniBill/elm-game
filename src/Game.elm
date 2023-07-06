module Game exposing (Model, Msg, Position, init, subscriptions, update, view)

import Browser.Events
import Ecs
import Ecs.Component as Component
import Ecs.Config as Config
import Ecs.Entity as Entity
import Ecs.System as System exposing (System)
import Effect exposing (Effect)
import Html exposing (Html)
import Length exposing (Length)
import Quantity


type alias Model =
    { ecsConfig : Ecs.Config
    , positionComponent : Ecs.Component Position
    }


ecsConfigSpec : Config.Spec Model
ecsConfigSpec =
    { get = .ecsConfig
    , set = \config world -> { world | ecsConfig = config }
    }


positionSpec : Component.Spec Position Model
positionSpec =
    { get = .positionComponent
    , set = \positionComponent world -> { world | positionComponent = positionComponent }
    }


positionSystem : System Model
positionSystem =
    System.map
        (\position ->
            let
                dx : Length
                dx =
                    Quantity.multiplyBy -0.01 position.x

                dy : Length
                dy =
                    Quantity.multiplyBy -0.01 position.y
            in
            { x = position.x |> Quantity.plus dx
            , y = position.y |> Quantity.plus dy
            }
        )
        positionSpec


type alias Position =
    { x : Length
    , y : Length
    }


type Msg
    = Tick


view : Model -> Html Msg
view _ =
    Html.text "..."


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        Tick ->
            ( model
                |> positionSystem
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrame (\_ -> Tick)


init : ( Model, Effect )
init =
    let
        ( _, model ) =
            { ecsConfig = Config.init
            , positionComponent = Component.empty
            }
                |> Entity.create ecsConfigSpec
                |> Entity.with
                    ( positionSpec
                    , { x = Length.meter
                      , y = Quantity.zero
                      }
                    )
    in
    ( model, Effect.none )
