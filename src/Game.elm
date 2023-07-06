module Game exposing (Flags, Model, Msg, Vector2d, World, init, subscriptions, time, update, view)

import Acceleration exposing (MetersPerSecondSquared)
import Browser.Events
import Circle2d exposing (Circle2d)
import Direction2d
import Duration exposing (Duration)
import Ecs
import Ecs.Component as Component
import Ecs.Config as Config
import Ecs.Entity as Entity
import Ecs.System as System exposing (System)
import Effect exposing (Effect)
import Html exposing (Html)
import Length exposing (Meters)
import Point2d
import Quantity
import Speed exposing (MetersPerSecond, Speed)
import Time
import Vector2d
import WebGL.Shape2d as Shape2d exposing (Color, rgb)
import WebGL.Shape2d.Render as Render exposing (Height, Width)
import WebGL.Shape2d.SolidShape as SolidShape exposing (SolidShape)


type alias Vector2d unit =
    Vector2d.Vector2d unit ()


type alias Point2d =
    Point2d.Point2d Meters ()


type alias Circle2d =
    Circle2d.Circle2d Meters ()


type alias Direction2d =
    Direction2d.Direction2d ()


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
    , positionComponent : Ecs.Component (Vector2d Meters)
    , speedComponent : Ecs.Component (Vector2d MetersPerSecond)
    , accelerationComponent : Ecs.Component (Vector2d MetersPerSecondSquared)
    }


ecsConfigSpec : Config.Spec World
ecsConfigSpec =
    { get = .ecsConfig
    , set = \config world -> { world | ecsConfig = config }
    }


positionSpec : Component.Spec (Vector2d Meters) World
positionSpec =
    { get = .positionComponent
    , set = \positionComponent world -> { world | positionComponent = positionComponent }
    }


speedSpec : Component.Spec (Vector2d MetersPerSecond) World
speedSpec =
    { get = .speedComponent
    , set = \speedComponent world -> { world | speedComponent = speedComponent }
    }


accelerationSpec : Component.Spec (Vector2d MetersPerSecondSquared) World
accelerationSpec =
    { get = .accelerationComponent
    , set = \accelerationComponent world -> { world | accelerationComponent = accelerationComponent }
    }


positionSystem : Duration -> System World
positionSystem deltaT =
    System.map2
        (\( speed, _ ) ( position, setPosition ) ->
            let
                deltaX : Vector2d Meters
                deltaX =
                    Vector2d.for deltaT speed
            in
            setPosition (Vector2d.plus deltaX position)
        )
        speedSpec
        positionSpec


speedSystem : Duration -> System World
speedSystem deltaT =
    System.map2
        (\( accelleration, _ ) ( speed, setSpeed ) ->
            let
                deltaV : Vector2d MetersPerSecond
                deltaV =
                    Vector2d.for deltaT accelleration

                newSpeed : Vector2d MetersPerSecond
                newSpeed =
                    speed
                        |> Vector2d.plus deltaV

                limiter : Speed
                limiter =
                    Quantity.min
                        (Vector2d.length newSpeed)
                        (Speed.kilometersPerHour 20)
            in
            newSpeed
                |> Vector2d.scaleTo limiter
                |> antiwiggle
                |> setSpeed
        )
        accelerationSpec
        speedSpec


{-| Force a very small vector to zero. Avoids "wiggles".
-}
antiwiggle : Vector2d.Vector2d units coordinates -> Vector2d.Vector2d units coordinates
antiwiggle vec =
    if abs (Quantity.unwrap (Vector2d.length vec)) < 0.01 then
        Vector2d.zero

    else
        vec


view : Model -> Html Msg
view model =
    let
        scale : Float
        scale =
            min model.width model.height / 200
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
    rectangle bgColor 200 200


bgColor : Color
bgColor =
    rgb 0 0 0


viewClipper : Model -> SolidShape
viewClipper _ =
    SolidShape.group
        [ rectangle clipColor 1000 1000
            |> Shape2d.move (-1100 + 1000 / 2) 0
        , rectangle clipColor 1000 1000
            |> Shape2d.move (1100 - 1000 / 2) 0
        , rectangle clipColor 1000 1000
            |> Shape2d.move 0 (-1100 + 1000 / 2)
        , rectangle clipColor 1000 1000
            |> Shape2d.move 0 (1100 - 1000 / 2)
        ]


clipColor : Color
clipColor =
    rgb 0 255 0


viewBall : Model -> SolidShape
viewBall ({ ball } as model) =
    let
        center : Point2d
        center =
            Component.get ball model.world.positionComponent
                |> Maybe.withDefault Vector2d.zero
                |> Vector2d.components
                |> (\( x, y ) -> Point2d.xy x y)
    in
    circle (rgb 255 0 0) (Circle2d.withRadius Length.meter center)


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
                |> Length.inMeters

        move : { x : Float, y : Float }
        move =
            Circle2d.centerPoint circ
                |> Point2d.toMeters
    in
    Render.circle color
        |> SolidShape.shape (r * 2) (r * 2)
        |> Shape2d.move move.x move.y


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        Tick now ->
            let
                world : World
                world =
                    model.world

                deltaT : Duration
                deltaT =
                    (Time.posixToMillis now - Time.posixToMillis model.now)
                        |> toFloat
                        |> Duration.milliseconds
            in
            ( { model
                | now = now
                , world =
                    { world
                        | accelerationComponent =
                            Component.set
                                model.ball
                                (gamepadToAccelleration model)
                                world.accelerationComponent
                    }
                        |> positionSystem deltaT
                        |> speedSystem deltaT
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


gamepadToAccelleration : Model -> Vector2d MetersPerSecondSquared
gamepadToAccelleration model =
    let
        direction : Direction2d
        direction =
            Direction2d.degrees <| toFloat (Time.posixToMillis model.now)
    in
    Vector2d.withLength (Acceleration.gees 1) direction


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
            , speedComponent = Component.empty
            , accelerationComponent = Component.empty
            }

        ( ball, world ) =
            initialWorld
                |> Entity.create ecsConfigSpec
                |> Entity.with ( positionSpec, Vector2d.zero )
                |> Entity.with ( speedSpec, Vector2d.zero )
                |> Entity.with ( accelerationSpec, Vector2d.zero )

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
