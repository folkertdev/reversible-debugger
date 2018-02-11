module Api exposing (initialize, step)

import Http exposing (Request)
import Json.Decode as Decode
import Json.Encode as Encode
import Types exposing (..)


type alias ApiRequest a =
    a -> Request (Result String a)


baseURL : String
baseURL =
    "/"


initialize : String -> Request ReplState
initialize programString =
    let
        url =
            baseURL ++ "initialize"
    in
        Http.post url (Http.jsonBody (Encode.string programString)) Types.decodeReplState


step : (Result Http.Error ReplState -> msg) -> Instruction -> ReplState -> Cmd msg
step toMsg instruction replState =
    let
        url =
            baseURL ++ "step"

        body =
            Encode.list
                [ Types.encodeInstruction instruction
                , Encode.bool True
                , Types.encodeReplState replState
                ]
    in
        Http.send toMsg <| Http.post url (Http.jsonBody body) Types.decodeReplState
