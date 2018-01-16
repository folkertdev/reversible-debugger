module Main exposing (..)

--

import Api
import Color exposing (..)
import Diagram
import Dict exposing (Dict)
import Element exposing (Element, column, el, row, text)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Examples
import FontAwesome
import Html exposing (Html)
import Http
import Style
import Style.Color as Color
import Style.Font as Font
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Types
    exposing
        ( Actor
        , Context
        , ExhaustableZipper(..)
        , GlobalAtom
        , GlobalType
        , Identifier(..)
        , Instruction(..)
        , LocalAtom(..)
        , LocalTypeState
        , PID(..)
        , ReplState
        , ThreadState(..)
        , unIdentifier
        )


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
    , Http.send InitialState (Api.initialize Examples.threeBuyer)
    )



-- UPDATE


type Msg
    = InitialState (Result Http.Error Current)
    | Step Instruction
    | Stepped (Result Http.Error Current)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialState (Ok newUrl) ->
            ( Model (Just newUrl), Cmd.none )

        InitialState (Err e) ->
            let
                _ =
                    Debug.log "initial error" e
            in
            ( model, Cmd.none )

        Step instruction ->
            case model.replState of
                Just replState ->
                    ( model
                    , Api.step Stepped instruction replState
                    )

                Nothing ->
                    ( model, Cmd.none )

        Stepped (Ok newUrl) ->
            ( Model (Just newUrl), Cmd.none )

        Stepped (Err e) ->
            let
                _ =
                    Debug.log "decoding error" e
            in
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
                        , row None [] (viewThreadState context threadState)
                        , row None [] [ Element.html (Svg.svg [ Svg.width "500", Svg.height "600" ] [ drawTypeState context ]) ]
                        ]
                    , column ContextBlock
                        [ width (percent 30) ]
                        (viewContext context)
                    ]


viewThreadState : Context -> ThreadState -> List (Element Style variation Msg)
viewThreadState context state =
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
        |> List.map (uncurry (viewThread context (Dict.size annotatedThreads)))


type ThreadActivity
    = Active
    | Inactive
    | Blocked
    | Uninitialized


viewThread : Context -> Int -> ThreadActivity -> Types.Thread -> Element Style variation Msg
viewThread context n activity thread =
    let
        (PID pid) =
            thread.pid

        { program, history } =
            thread

        localTypeState =
            lookupTypeState thread.actor context

        header =
            case localTypeState of
                Nothing ->
                    "Not a protocol member"

                Just { participant } ->
                    "Participating as `" ++ unIdentifier participant ++ "`"
    in
    Element.paragraph (ThreadBlock activity)
        [ minWidth (px 50), width (percent (1 / toFloat n * 100)) ]
        [ Element.row None
            [ width (percent 100), spread ]
            [ Element.button Button [ onClick (Step (Back thread.pid)), alignLeft ] (el None [] (icon FontAwesome.arrow_left 20))
            , el None [] (Element.text header)
            , Element.button Button [ onClick (Step (Forth thread.pid)), alignRight ] (el None [] (icon FontAwesome.arrow_right 20))
            ]
        , Element.row None
            [ spread ]
            [ lookupTypeState thread.actor context
                |> Maybe.map viewLocalTypeState
                |> Maybe.withDefault Element.empty
            , Element.text (toString thread.program)
            ]
        ]


viewContext : Context -> List (Element Style variation Msg)
viewContext context =
    [ Element.bold <| "Variable Count: " ++ toString context.variableCount
    , Element.bold "Variables"
    , viewBindings context.bindings
    , Element.bold "Channels"
    , viewChannels context.channels
    , Element.bold "LocalTypeState"
    , Element.text (toString context.localTypeStates)
    ]


lookupTypeState : Actor -> Context -> Maybe Types.LocalTypeState
lookupTypeState actor { participantMap, localTypeStates } =
    List.head actor
        |> Maybe.andThen (\(Types.Identifier i) -> Dict.get i localTypeStates)



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
viewChannels channles =
    channles
        |> Dict.toList
        |> List.map (uncurry (viewBinding (\v -> text (toString v))))
        |> Element.textLayout None []


viewLocalTypeState : LocalTypeState -> Element Style variation msg
viewLocalTypeState { participant, state } =
    viewExhaustableZipper state


viewExhaustableZipper : ExhaustableZipper (LocalAtom String) -> Element Style variation msg
viewExhaustableZipper zipper =
    case zipper of
        Empty ->
            text "Empty"

        Exhausted _ ->
            text "Exhausted"

        Zipper ( p, c, n ) ->
            text (viewLocalAtom c)


viewLocalAtom : LocalAtom String -> String
viewLocalAtom atom =
    case atom of
        LocalAtomSend { receiver, type_ } ->
            "Send to " ++ unIdentifier receiver ++ " of type " ++ type_

        LocalAtomReceive { sender, type_ } ->
            "Receive from " ++ unIdentifier sender ++ " of type " ++ type_



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP
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


drawTypeState : { a | localTypeStates : Dict String LocalTypeState, globalType : GlobalType } -> Svg msg
drawTypeState { localTypeStates, globalType } =
    let
        { parameters, atoms } =
            globalType
    in
    Diagram.drawSegments (List.map unIdentifier parameters) globalType { width = 500, height = 500, headerHeight = 20 }
