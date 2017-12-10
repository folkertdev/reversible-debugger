module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Types
import Dict


type alias Current =
    Types.ReplState


main =
    Html.program
        { init = init "cats"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { replState : Maybe Current }


init : String -> ( Model, Cmd Msg )
init topic =
    ( Model Nothing
    , getRandomGif "skip"
    )



-- UPDATE


type Msg
    = MorePlease
    | InitialState (Result Http.Error Current)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getRandomGif "skip" )

        InitialState (Ok newUrl) ->
            Debug.log "ok" ( Model (Just newUrl), Cmd.none )

        InitialState (Err e) ->
            Debug.log ("err" ++ toString e) ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , br [] []
        , case model.replState of
            Nothing ->
                text ""

            Just { context, threadState } ->
                viewContext context
        ]


viewContext { bindings } =
    bindings
        |> Dict.toList
        |> List.map (Tuple.mapSecond viewValue)
        |> List.map viewBinding
        |> List.map (\v -> li [] [ v ])
        |> ul []


viewBinding ( key, value ) =
    label [] [ b [] [ text key ], value ]


viewValue value =
    case value of
        _ ->
            text (toString value)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "http://localhost:8081/initialize"
    in
        Http.send InitialState (Http.post url (Http.jsonBody (Encode.string threeBuyer)) Types.decodeReplState)


threeBuyer =
    """global type ThreeBuyer A B C V =
        A -> V: <title>.V -> {A, B} : <price>.A -> B : <share>.
            B -> {A, V} : <OK>.
            B -> C: <share>. B -> C : <thunk>.
            B -> V : <address>. V -> B : <date>. end

session ThreeBuyer Alice Bob Carol Vendor where

let d = port in

// A
thread Alice where
    let title = 42 in
        {send d title} ;
        let price = {receive d} in
            {send d share};
            let ok = {receive d} in
                skip
            end
        end
    end
end;

// B
thread Bob where
    let ok = 1 in
        let address = 8 in
            let price = {receive d} in
                let share = {receive d} in
                    let remainder = proc { x }
                            {send d address};
                            let date = {receive d} in
                                skip
                            end
                         end
                    in
                        {send d ok};
                        {send d ok};
                        {send d share};
                        {send d remainder}
                    end
                end
            end
        end
    end
end;

thread Carol where
    let unit = 0 in
        let share = {receive d} in
            let remainder = {receive d} in
                {remainder unit}
            end
        end
    end
end;



thread Vendor where
    let price = 5 in
        let date = 0 in
            let title = {receive d} in
                // send to A
                {send d price} ;
                // send to B
                {send d price} ;
                let ok = {receive d} in
                    let address = {receive d} in
                        {send d date}
                    end
                end
            end
        end
    end
end

// channel d
end

// session where block
end
"""
