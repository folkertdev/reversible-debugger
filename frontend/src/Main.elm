module Main exposing (..)

--

import Color exposing (..)
import Dict exposing (Dict)
import Element exposing (Element, column, el, row, text)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import FontAwesome
import Html exposing (Html)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Style
import Style.Color as Color
import Style.Font as Font
import ThreeBuyer
import Types exposing (Context, Instruction(..), PID(..), ReplState, ThreadState(..))


icon awesome size =
    Element.html (awesome Color.white size)


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
    | Step Instruction
    | Stepped (Result Http.Error Current)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getRandomGif "skip" )

        InitialState (Ok newUrl) ->
            ( Model (Just newUrl), Cmd.none )

        InitialState (Err e) ->
            ( model, Cmd.none )

        Step instruction ->
            case model.replState of
                Just replState ->
                    ( model
                    , step instruction replState
                    )

                Nothing ->
                    ( model, Cmd.none )

        Stepped (Ok newUrl) ->
            ( Model (Just newUrl), Cmd.none )

        Stepped (Err e) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout stylesheet <|
        case model.replState of
            Nothing ->
                Element.empty

            Just { context, threadState } ->
                row None
                    []
                    [ column ThreadStateBlock
                        [ width (percent 70) ]
                        [ Element.button None [ onClick (Step SkipLets) ] (text "Normalize!")
                        , row None [] (viewThreadState threadState)
                        ]
                    , column ContextBlock
                        [ width (percent 30) ]
                        (viewContext context)
                    ]


viewThreadState : ThreadState -> List (Element Style variation Msg)
viewThreadState state =
    let
        threads =
            case state of
                Running ({ pid } as current) others ->
                    let
                        (Types.PID pid_) =
                            pid
                    in
                    { others | active = Dict.insert pid_ current others.active }

                Stuck other ->
                    other

        annotatedThreads =
            List.foldl Dict.union
                Dict.empty
                [ threads.active
                    |> Dict.map (\k v -> ( Active, v ))
                , threads.inactive
                    |> Dict.map (\k v -> ( Inactive, v ))
                , threads.blocked
                    |> Dict.map (\k v -> ( Blocked, v ))
                , threads.uninitialized
                    |> Dict.map (\k v -> ( Uninitialized, v ))
                ]
    in
    annotatedThreads
        |> Dict.values
        |> List.map (uncurry (viewThread (Dict.size annotatedThreads)))


type ThreadActivity
    = Active
    | Inactive
    | Blocked
    | Uninitialized


viewThread : Int -> ThreadActivity -> Types.Thread -> Element Style variation Msg
viewThread n activity thread =
    let
        (PID pid) =
            thread.pid

        { program, history } =
            thread
    in
    Element.paragraph (ThreadBlock activity)
        [ minWidth (px 50), width (percent (1 / toFloat n * 100)) ]
        [ Element.paragraph None
            [ width (percent 100) ]
            [ Element.button Button [ onClick (Step (Back thread.pid)), alignLeft ] (el None [] (icon FontAwesome.arrow_left 20))
            , Element.button Button [ onClick (Step (Forth thread.pid)), alignRight ] (el None [] (icon FontAwesome.arrow_right 20))
            ]
        , text <| toString pid ++ " => " ++ toString program
        ]


viewContext : Context -> List (Element Style variation Msg)
viewContext context =
    [ Element.bold "Variables"
    , viewBindings context.bindings
    , Element.bold "Channels"
    , viewChannels context.channels
    ]



{-
   column None
       []
       , viewBindings context.channels
       ]
-}


viewBindings : Dict String a -> Element Style variation msg
viewBindings bindings =
    bindings
        |> Dict.toList
        |> List.map (uncurry (viewBinding (\v -> text (toString v))))
        |> Element.textLayout None []


viewBinding : (value -> Element Style variation msg) -> String -> value -> Element Style variation msg
viewBinding viewValue key value =
    Element.paragraph None
        []
        [ Element.bold key
        , Element.bold " => "
        , viewValue value
        ]


viewChannels : Dict String a -> Element Style variation msg
viewChannels bindings =
    bindings
        |> Dict.toList
        |> List.map (uncurry (viewBinding (\v -> text (toString v))))
        |> Element.textLayout None []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


baseURL =
    "/"


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            baseURL ++ "initialize"
    in
    Http.send InitialState (Http.post url (Http.jsonBody (Encode.string ThreeBuyer.threeBuyer)) Types.decodeReplState)


forth : Types.PID -> Types.Instruction
forth =
    Types.Forth


step : Types.Instruction -> Types.ReplState -> Cmd Msg
step instruction replState =
    let
        url =
            baseURL ++ "step"

        body =
            Encode.list
                [ Types.encodeInstruction instruction
                , Types.encodeReplState replState
                ]
    in
    Http.send Stepped (Http.post url (Http.jsonBody body) Types.decodeReplState)



-- We need a type that represents out style identifiers.
-- These act like css classes


type Style
    = Title
    | Button
    | ThreadStateBlock
    | ContextBlock
    | ThreadBlock ThreadActivity
    | None



-- We define our stylesheet


stylesheet =
    Style.styleSheet
        [ Style.style Title
            [ Color.text darkGrey
            , Color.background white
            , Font.size 5 -- all units given as px
            ]
        , Style.style (ThreadBlock Active)
            [ Color.background palette.green
            ]
        , Style.style (ThreadBlock Blocked)
            [ Color.background palette.orange
            ]
        , Style.style (ThreadBlock Inactive)
            [ Color.background palette.blue
            ]
        , Style.style (ThreadBlock Uninitialized)
            [ Color.background palette.yellow
            ]
        ]


palette =
    { red = Color.rgb 255 179 186
    , orange = Color.rgb 255 223 186
    , yellow = Color.rgb 255 255 186
    , green = Color.rgb 186 255 201
    , blue = Color.rgb 186 225 255
    }



-- Element.layout renders the elements as html.
-- Every layout requires a stylesheet.
