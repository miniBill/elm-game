module Game exposing (Flags, Model, Msg, World, init, subscriptions, time, update, view)

import Browser.Events
import Circle2d exposing (Circle2d)
import Ecs
import Ecs.Component as Component
import Ecs.Config as Config
import Ecs.Entity as Entity
import Ecs.System as System exposing (System)
import Effect exposing (Effect)
import Html exposing (Html)
import Point2d exposing (Point2d)
import Time
import Vector2d exposing (Vector2d)
import WebGL.Shape2d as Shape2d exposing (Color, rgb)
import WebGL.Shape2d.Render as Render exposing (Height, Width)
import WebGL.Shape2d.SolidShape as SolidShape exposing (SolidShape)


type alias Flags =
    { now : Time.Posix
    , width : Width
    , height : Height
    }


type Msg
    = Tick Time.Posix
    | Resize Int Int


type alias Model =
    { world : World
    , ball : Ecs.Entity
    , now : Time.Posix
    , width : Width
    , height : Height
    }


type alias World =
    { ecsConfig : Ecs.Config
    , positionComponent : Ecs.Component Vector2d
    , velocityComponent : Ecs.Component Vector2d
    }


ecsConfigSpec : Config.Spec World
ecsConfigSpec =
    { get = .ecsConfig
    , set = \config world -> { world | ecsConfig = config }
    }


positionSpec : Component.Spec Vector2d World
positionSpec =
    { get = .positionComponent
    , set = \positionComponent world -> { world | positionComponent = positionComponent }
    }


velocitySpec : Component.Spec Vector2d World
velocitySpec =
    { get = .velocityComponent
    , set = \velocityComponent world -> { world | velocityComponent = velocityComponent }
    }


positionSystem : System World
positionSystem =
    System.map2
        (\( velocity, _ ) ( position, setPosition ) ->
            setPosition (Vector2d.sum velocity position)
        )
        velocitySpec
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
            [ viewBackground model
            , viewBall model
            , viewClipper model
            ]
                |> SolidShape.group
                |> Shape2d.scale scale scale
                |> List.singleton
                |> SolidShape.toEntities model
        }


viewBackground : Model -> SolidShape
viewBackground _ =
    rectangle bgColor 2 2


bgColor : Color
bgColor =
    rgb 0 0 0


viewClipper : Model -> SolidShape
viewClipper _ =
    SolidShape.group
        [ rectangle bgColor 100 100
            |> Shape2d.move (-101 + 100 / 2) 0
        , rectangle bgColor 100 100
            |> Shape2d.move (101 - 100 / 2) 0
        , rectangle bgColor 100 100
            |> Shape2d.move 0 (-101 + 100 / 2)
        , rectangle bgColor 100 100
            |> Shape2d.move 0 (101 - 100 / 2)
        ]


viewBall : Model -> SolidShape
viewBall ({ ball } as model) =
    let
        center : Point2d
        center =
            Component.get ball model.world.positionComponent
                |> Maybe.withDefault Vector2d.zero
                |> Vector2d.components
                |> Point2d.fromCoordinates
    in
    circle (rgb 255 0 0) (Circle2d.withRadius 0.1 center)


rectangle : Color -> Width -> Height -> SolidShape
rectangle color w h =
    Render.rect color
        |> SolidShape.shape w h


circle : Color -> Circle2d -> SolidShape
circle color circ =
    let
        r : Float
        r =
            Circle2d.radius circ

        ( dx, dy ) =
            Circle2d.centerPoint circ
                |> Point2d.coordinates
    in
    Render.circle color
        |> SolidShape.shape (r * 2) (r * 2)
        |> Shape2d.move dx dy


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

        Resize w h ->
            ( { model
                | width = toFloat w
                , height = toFloat h
              }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame Tick
        , Browser.Events.onResize Resize
        ]


init : Flags -> ( Model, Effect )
init flags =
    let
        initialWorld : World
        initialWorld =
            { ecsConfig = Config.init
            , positionComponent = Component.empty
            , velocityComponent = Component.empty
            }

        ( ball, world ) =
            initialWorld
                |> Entity.create ecsConfigSpec
                |> Entity.with
                    ( positionSpec
                    , Vector2d.fromComponents ( 1, 0 )
                    )
                |> Entity.with
                    ( velocitySpec
                    , Vector2d.fromComponents ( 0, 0 )
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
