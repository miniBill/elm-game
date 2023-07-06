module Game exposing (Flags, Model, Msg, Position, World, init, subscriptions, time, update, view)

import Browser.Events
import Ecs
import Ecs.Component as Component
import Ecs.Config as Config
import Ecs.Entity as Entity
import Ecs.System as System exposing (System)
import Effect exposing (Effect)
import Html exposing (Html)
import Math.Vector3 exposing (Vec3)
import Time
import WebGL.Shape2d as Shape2d exposing (rgb)
import WebGL.Shape2d.Render as Render exposing (Height, Width)
import WebGL.Shape2d.SolidShape as SolidShape exposing (SolidShape)


type alias Flags =
    { now : Time.Posix
    , width : Width
    , height : Height
    }


type Msg
    = Tick Time.Posix


type alias Model =
    { world : World
    , ball : Ecs.Entity
    , now : Time.Posix
    , width : Width
    , height : Height
    }


type alias World =
    { ecsConfig : Ecs.Config
    , positionComponent : Ecs.Component Position
    }


type alias Position =
    { x : Float
    , y : Float
    }


ecsConfigSpec : Config.Spec World
ecsConfigSpec =
    { get = .ecsConfig
    , set = \config world -> { world | ecsConfig = config }
    }


positionSpec : Component.Spec Position World
positionSpec =
    { get = .positionComponent
    , set = \positionComponent world -> { world | positionComponent = positionComponent }
    }


positionSystem : System World
positionSystem =
    System.map
        (\position ->
            let
                antiwiggle : Float -> Float
                antiwiggle x =
                    if abs x < 0.0001 then
                        0

                    else
                        x

                dx : Float
                dx =
                    max 0 <| antiwiggle <| -0.01 * position.x

                dy : Float
                dy =
                    antiwiggle <| -0.01 * position.y
            in
            { x = position.x + dx
            , y = position.y + dy
            }
        )
        positionSpec


view : Model -> Html Msg
view model =
    let
        scale : Float
        scale =
            min model.width model.height / 2
    in
    Shape2d.view
        { screen = model
        , entities =
            [ viewBall model
            , viewEdge model
            ]
                |> SolidShape.group
                |> Shape2d.scale scale scale
                |> List.singleton
                |> SolidShape.toEntities model
        }


viewEdge : Model -> SolidShape
viewEdge _ =
    rectangle (rgb 0 0 0) 2 2


viewBall : Model -> SolidShape
viewBall ({ ball } as model) =
    let
        ( dx, dy ) =
            case Component.get ball model.world.positionComponent of
                Nothing ->
                    ( 0, 0 )

                Just { x, y } ->
                    ( x, y )
    in
    rectangle (rgb 255 0 0) 0.2 0.2
        |> Shape2d.move dx dy


rectangle : Vec3 -> Width -> Height -> SolidShape
rectangle color w h =
    Render.rect color
        |> SolidShape.shape w h


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        Tick now ->
            ( { model
                | now = now
                , world =
                    model.world
                        |> positionSystem
              }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrame Tick


init : Flags -> ( Model, Effect )
init flags =
    let
        ( ball, world ) =
            { ecsConfig = Config.init
            , positionComponent = Component.empty
            }
                |> Entity.create ecsConfigSpec
                |> Entity.with
                    ( positionSpec
                    , { x = 1
                      , y = 0
                      }
                    )

        model : Model
        model =
            { world = world
            , ball = ball
            , now = flags.now
            , width = flags.width
            , height = flags.height
            }
    in
    ( model
    , Effect.none
    )


time : Model -> Time.Posix
time { now } =
    now
