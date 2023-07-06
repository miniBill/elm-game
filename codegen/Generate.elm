module Generate exposing (main)

{-| -}

import Elm exposing (File)
import Gen.CodeGen.Generate as Generate
import Json.Decode
import Json.Encode
import List.Extra


main : Program Json.Encode.Value () ()
main =
    Generate.fromJson
        (Json.Decode.list decodeFile)
        (\files ->
            List.map
                (\( first, rest ) ->
                    directoryToGen first.directory (first :: rest)
                )
                (List.Extra.gatherEqualsBy .directory files)
        )


type alias InputFile =
    { directory : String
    , filename : String
    , contents : String
    }


decodeFile : Json.Decode.Decoder InputFile
decodeFile =
    Json.Decode.map2
        (\path contents ->
            let
                splat : List String
                splat =
                    String.split "/" path

                directory : String
                directory =
                    splat
                        |> List.drop 1
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse
                        |> String.join "/"

                filename : String
                filename =
                    splat
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault "???"
            in
            { directory = directory
            , filename = filename
            , contents = contents
            }
        )
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "contents" Json.Decode.string)


directoryToGen : String -> List InputFile -> File
directoryToGen moduleName files =
    files
        |> List.map fileToGen
        |> Elm.file [ moduleName ]


fileToGen : InputFile -> Elm.Declaration
fileToGen { filename, contents } =
    contents
        |> Elm.string
        |> Elm.declaration (toName filename)
        |> Elm.expose


toName : String -> String
toName f =
    let
        cleaned =
            f
                |> String.split "."
                |> List.reverse
                |> List.drop 1
                |> List.reverse
                |> String.join "."
    in
    case String.uncons cleaned of
        Nothing ->
            cleaned

        Just ( first, _ ) ->
            if Char.isDigit first then
                "i_" ++ cleaned

            else
                cleaned
