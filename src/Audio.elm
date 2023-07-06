module Audio exposing (Partial, destination, gain, note)

import WebAudio
import WebAudio.Property


type alias Partial =
    List WebAudio.Node -> WebAudio.Node


gain : Float -> Partial -> Partial
gain value =
    partial WebAudio.gain [ WebAudio.Property.gain value ]


delay : Float -> Partial -> Partial
delay value =
    partial WebAudio.delay [ WebAudio.Property.delayTime value ]


partial :
    (List WebAudio.Property.Property -> Partial)
    -> List WebAudio.Property.Property
    -> Partial
    -> Partial
partial ctor attrs previous next =
    previous [ ctor attrs next ]


destination : Partial -> WebAudio.Node
destination next =
    next [ WebAudio.audioDestination ]


note : Int -> Float
note n =
    440 * 2 ^ toFloat n
