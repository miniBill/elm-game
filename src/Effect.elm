module Effect exposing (Effect(..), none, toCmd)


type Effect
    = Batch (List Effect)


none : Effect
none =
    batch []


batch : List Effect -> Effect
batch =
    Batch


toCmd : Effect -> Cmd msg
toCmd effect =
    case effect of
        Batch children ->
            Cmd.batch (List.map toCmd children)
