module Main exposing (..)

--

import Api
import Color exposing (..)
import Color.Manipulate as Manipulate
import Diagram
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Examples
import FontAwesome
import Html exposing (Html)
import Html.Attributes
import Http
import MappingTable exposing (Msg, State(Closed, Open), toggle, view)
import SidePane
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Types
    exposing
        ( Actor
        , Context
        , ExhaustableZipper(..)
        , GlobalAtom
        , GlobalType
        , Identifier
        , Instruction(..)
        , LocalAtom(..)
        , LocalTypeState
        , PID(..)
        , Participant(..)
        , ReplState
        , ThreadState(..)
        , unParticipant
        )


unIdentifier =
    identity


icon awesome size =
    Element.html (awesome Color.white size)


main =
    Html.program
        { init = init "cats"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Example
    = ThreeBuyer
    | AsynchAnd


type alias Model =
    { replState : Maybe Types.ReplState
    , example : Example
    , variables : MappingTable.State
    , channels : MappingTable.State
    , controls : MappingTable.State
    , localTypeStates : MappingTable.State
    }


init : String -> ( Model, Cmd Msg )
init topic =
    ( { replState = Nothing
      , example = ThreeBuyer
      , variables = MappingTable.Open
      , channels = MappingTable.Closed
      , localTypeStates = MappingTable.Closed
      , controls = MappingTable.Open
      }
    , Http.send InitialState (Api.initialize Examples.threeBuyer)
    )



-- UPDATE


type Msg
    = InitialState (Result Http.Error Types.ReplState)
    | Step Instruction
    | Stepped (Result Http.Error Types.ReplState)
    | SwitchExample Example
    | Toggle SidePane.DropDown MappingTable.Msg


sidePaneConfig : SidePane.Config Msg
sidePaneConfig =
    { toggle = Toggle
    , instruction = Step
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle dropdown msg ->
            ( SidePane.update dropdown model, Cmd.none )

        InitialState (Ok newUrl) ->
            ( { model | replState = Just newUrl }, Cmd.none )

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
            ( { model | replState = Just newUrl }, Cmd.none )

        Stepped (Err e) ->
            let
                _ =
                    Debug.log "decoding error" e
            in
            ( model, Cmd.none )

        SwitchExample example ->
            if model.example == example then
                ( model, Cmd.none )
            else
                let
                    exampleString =
                        case example of
                            ThreeBuyer ->
                                Examples.threeBuyer

                            AsynchAnd ->
                                Examples.asynchAnd
                in
                ( { model | example = example }, Http.send InitialState (Api.initialize exampleString) )



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] <|
        case model.replState of
            Nothing ->
                Element.empty

            Just { context, threadState } ->
                let
                    typeStateUI =
                        Dict.toList context.localTypeStates
                            |> List.concatMap
                                (\( name, state ) ->
                                    [ row [] [ text name ]
                                    , row [] [ viewLocalTypeState state ]
                                    ]
                                )
                in
                row
                    [ width fill, height fill ]
                    [ column
                        [ width (fillPortion 7)
                        ]
                      <|
                        [ row [] (viewThreadState context threadState)
                        , row []
                            [ Element.html (Svg.svg [ Svg.width "500", Svg.height "600" ] [ drawTypeState context ]) ]
                        ]
                            ++ typeStateUI
                    , column
                        [ width (fillPortion 3)
                        , Border.color (Manipulate.darken 0.2 palette.grey)
                        , Border.widthEach { bottom = 0, left = 2, right = 0, top = 0 }
                        ]
                        (SidePane.sidepane sidePaneConfig model context)
                    ]


viewThreadState : Context -> ThreadState -> List (Element Msg)
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


viewThread : Context -> Int -> ThreadActivity -> Types.Thread -> Element Msg
viewThread context n activity thread =
    let
        (PID pid) =
            thread.pid

        { program, history } =
            thread

        localTypeState =
            lookupTypeState thread.actor context

        header =
            text <|
                case localTypeState of
                    Nothing ->
                        "Not a protocol member"

                    Just { participant } ->
                        "Participating as `" ++ unParticipant participant ++ "`"
    in
    Element.paragraph
        [ width fill
        , height (px 20)
        , Border.widthEach { bottom = 0, left = 0, right = 0, top = 5 }
        , Border.color (threadActivityToColor activity)
        , Font.center
        , center
        ]
        [ Element.Input.button [ alignLeft, Background.color palette.grey, width shrink ]
            { onPress = Just (Step (Back thread.pid)), label = icon FontAwesome.arrow_left 20 }
        , Element.el [ width fill ] header
        , Element.Input.button [ alignRight, Background.color palette.grey, width shrink ]
            { onPress = Just (Step (Forth thread.pid)), label = icon FontAwesome.arrow_right 20 }
        ]


bold : String -> Element msg
bold content =
    Element.el [ Font.bold ] (text content)


lookupTypeState : Actor -> Context -> Maybe Types.LocalTypeState
lookupTypeState actor { localTypeStates } =
    List.head actor
        |> Maybe.andThen (\i -> Dict.get (unParticipant i) localTypeStates)



{-
   column None
       []
       , viewBindings context.channels
       ]
-}


viewBindings : Dict String a -> Element msg
viewBindings bindings =
    bindings
        |> Dict.toList
        |> List.map (uncurry (viewBinding (\v -> text (toString v))))
        |> Element.paragraph []


viewBinding : (value -> Element msg) -> String -> value -> Element msg
viewBinding viewValue key value =
    Element.paragraph
        []
        [ bold key
        , bold " => "
        , viewValue value
        ]


viewChannels : Dict String a -> Element msg
viewChannels channles =
    channles
        |> Dict.toList
        |> List.map (uncurry (viewBinding (\v -> text (toString v))))
        |> Element.paragraph []


viewLocalTypeState : LocalTypeState -> Element msg
viewLocalTypeState { participant, state } =
    viewCarousel state


emptySet =
    "∅"


viewCarousel zipper =
    case zipper of
        Empty ->
            Element.row
                [ center, width fill ]
                [ Element.column [ width (fillPortion 3) ] []
                , Element.column [ width (fillPortion 1) ] [ text emptySet ]
                , Element.column [ width (fillPortion 3) ] []
                ]

        ExhaustedForward previous current ->
            text ""

        ExhaustedBackward current next ->
            text ""

        Zipper ( p, c, n ) ->
            Element.row
                [ width fill ]
                [ Element.el
                    [ width (fillPortion 3) ]
                    (Element.row [ spacing 10 ] (List.map (viewAtom Right) <| List.reverse p))
                , Element.el
                    [ width (fillPortion 1)
                    ]
                    (viewAtom Center c)
                , Element.el
                    [ width (fillPortion 3) ]
                    (Element.row [ spacing 10 ] (List.map (viewAtom Left) n))
                ]


type Alignment
    = Left
    | Right
    | Center


viewAtom alignment atom =
    let
        textAlignMiddle =
            Element.attribute (Html.Attributes.style [ ( "text-align", "middle" ) ])

        shared =
            [ height <|
                case alignment of
                    Center ->
                        px 40

                    _ ->
                        px 30
            , case alignment of
                Left ->
                    alignLeft

                Right ->
                    alignRight

                Center ->
                    center
            , width (px 70)
            , Background.color palette.grey
            , Border.color (Manipulate.darken 0.2 palette.grey)
            , Border.width 2
            , Font.center
            ]

        label =
            case atom of
                LocalAtomReceive { sender, type_ } ->
                    "receive"

                LocalAtomSend { receiver, type_ } ->
                    "send"

                Choice _ _ ->
                    "choice"

                Select _ _ ->
                    "select"

                End ->
                    "end"
    in
    el shared
        (el [ textAlignMiddle ] (text label))


viewLocalTypeState_ : LocalTypeState -> Element msg
viewLocalTypeState_ { participant, state } =
    viewExhaustableZipper state


viewExhaustableZipper : ExhaustableZipper (LocalAtom String) -> Element msg
viewExhaustableZipper zipper =
    case zipper of
        Empty ->
            text "Empty"

        ExhaustedForward _ _ ->
            text "ExhaustedForward"

        ExhaustedBackward _ _ ->
            text "ExhaustedBackward"

        Zipper ( p, c, n ) ->
            text (viewLocalAtom c)


viewLocalAtom : LocalAtom String -> String
viewLocalAtom atom =
    case atom of
        LocalAtomSend { receiver, type_ } ->
            "Send to " ++ unIdentifier receiver ++ " of type " ++ type_

        LocalAtomReceive { sender, type_ } ->
            "Receive from " ++ unIdentifier sender ++ " of type " ++ type_

        Choice left right ->
            "Choice between " ++ viewLocalAtom left ++ " and " ++ viewLocalAtom right

        Select left right ->
            "Selection of " ++ viewLocalAtom left ++ " and " ++ viewLocalAtom right

        End ->
            "End"



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


threadActivityToColor : ThreadActivity -> Color
threadActivityToColor typeState =
    case typeState of
        Active ->
            palette.green

        Blocked ->
            palette.orange

        Inactive ->
            palette.blue

        Uninitialized ->
            palette.yellow



-- We define our stylesheet
{-
   stylesheet =
       Style.styleSheet
           [ Style.style Title
               [ Color.text darkGrey
               , Color.background white
               , Font.size 5

               -- all units given as px
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
-}


palette =
    { red = Color.rgb 255 179 186
    , orange = Color.rgb 255 223 186
    , yellow = Color.rgb 255 255 186
    , green = Color.rgb 186 255 201
    , blue = Color.rgb 186 225 255
    , grey = Color.rgb 132 141 130
    , navy = Color.rgb 44 59 99
    , midnight = Color.rgb 37 3 82
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
