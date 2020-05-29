module Ascii exposing (..)

import Browser
import Browser.Events
import Bytes exposing (Bytes)
import Color exposing (..)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Image exposing (..)
import Image.Color
import Json.Decode as Decode
import Task



------ MAIN ------


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



------ MODEL ------


type alias Model =
    { file : List File -- list or singular?
    , fileBytes : List Bytes
    , convertedASCII : String -- List String?
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { file = []
    , fileBytes = []
    , convertedASCII = ""
    }



------ UPDATE ------


type Msg
    = GotFiles (List File)
    | GotBytes (List Bytes)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFiles files ->
            ( { model | file = files }
            , Task.perform GotBytes <|
                Task.sequence <|
                    List.map File.toBytes files
            )

        GotBytes bytes ->
            let
                _ =
                    Debug.log "raw" (colorsToFloats (imageToColors (List.head bytes)))
            in
            ( { model | fileBytes = bytes }
            , Cmd.none
            )



------ SUBSCRIPTIONS ------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



------ VIEW ------


view : Model -> Html Msg
view model =
    div []
        [ input
            [ Attr.type_ "file"
            , Attr.multiple True
            , on "change" (Decode.map GotFiles filesDecoder)
            ]
            []
        ]



------ HELPERS ------


filesDecoder : Decode.Decoder (List File)
filesDecoder =
    Decode.at [ "target", "files" ] (Decode.list File.decoder)


imageToColors : Maybe Bytes -> List (List Color)
imageToColors file =
    let
        empty =
            [ [] ]
    in
    case file of
        Nothing ->
            let
                _ =
                    Debug.log "No file" empty
            in
            empty

        Just bytes ->
            let
                img =
                    Image.decode bytes
            in
            case img of
                Nothing ->
                    let
                        _ =
                            Debug.log "No decode" empty
                    in
                    empty

                Just i ->
                    let
                        col =
                            Image.Color.toList2d i

                        _ =
                            Debug.log "Image" col
                    in
                    col


colorToFloat : Color -> Float
colorToFloat col =
    let
        { red, green, blue, alpha } =
            Color.toRgba col
    in
    -- standardized on interval [0, 1]
    0.2126 * red + 0.7152 * green + 0.0722 * blue


colorsToFloats : List (List Color) -> List (List Float)
colorsToFloats cols =
    List.map (\xs -> List.map colorToFloat xs) cols
