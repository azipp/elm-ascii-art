module Ascii exposing (..)

import Browser
import Browser.Events
import Bytes exposing (Bytes)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Image exposing (..)
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
                    Debug.log "val" (image2pix (List.head bytes))
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


filesDecoder : Decode.Decoder (List File)
filesDecoder =
    Decode.at [ "target", "files" ] (Decode.list File.decoder)


image2pix : Maybe Bytes -> String
image2pix file =
    case file of
        Nothing ->
            "No file"

        Just i ->
            let
                im =
                    Image.decode i
            in
            case im of
                Nothing ->
                    "No decode"

                _ ->
                    "Image"
