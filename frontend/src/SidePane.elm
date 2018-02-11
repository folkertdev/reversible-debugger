module SidePane exposing (sidepane, update, Config, DropDown)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Input
import Element.Font as Font
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
import MappingTable exposing (State(Open, Closed), view, toggle, Msg)


type Either a b
    = Left a
    | Right b


update : DropDown -> HasSidePane a -> HasSidePane a
update dropdown model =
    case dropdown of
        Variables ->
            { model | variables = MappingTable.toggle model.variables }

        Controls ->
            { model | controls = MappingTable.toggle model.controls }

        LocalTypeStates ->
            { model | localTypeStates = MappingTable.toggle model.localTypeStates }

        Channels ->
            { model | channels = MappingTable.toggle model.channels }


type DropDown
    = Variables
    | Channels
    | Controls
    | LocalTypeStates


channels : MappingTable.State -> Dict Identifier ( Participant, Types.Queue ) -> Element Msg
channels state bindings =
    let
        records : List { key : Identifier, creator : Participant, value : Types.Queue }
        records =
            Dict.toList bindings
                |> List.map (\( key, ( creator, value ) ) -> { key = key, creator = creator, value = value })

        key : Element.Column { key : Identifier, creator : Participant, value : Types.Queue } msg
        key =
            { header = Element.el [ alignLeft, Font.bold ] (text "Key")
            , view = \record -> Element.map Basics.never (text record.key)
            }

        value : Element.Column { key : Identifier, creator : Participant, value : Types.Queue } msg
        value =
            { header = Element.el [ alignLeft, Font.bold, width fill ] (text "Value")
            , view = \record -> Element.map Basics.never (Element.paragraph [] [ text (toString record.value) ])
            }

        creator : Element.Column { key : Identifier, creator : Participant, value : Types.Queue } msg
        creator =
            { header = Element.el [ alignLeft, Font.bold ] (text "Creator")
            , view = \record -> Element.map Basics.never (text (unParticipant record.creator))
            }

        table : Element.Table { key : Identifier, creator : Participant, value : Types.Queue } msg
        table =
            { data = records
            , columns = [ key, value, creator ]
            }
    in
        MappingTable.viewCustom state "Channels" table


variables : MappingTable.State -> Dict Identifier ( Participant, Types.Value ) -> Element Msg
variables state bindings =
    let
        records : List { key : Identifier, creator : Participant, value : Types.Value }
        records =
            Dict.toList bindings
                |> List.map (\( key, ( creator, value ) ) -> { key = key, creator = creator, value = value })

        key : Element.Column { key : Identifier, creator : Participant, value : Types.Value } msg
        key =
            { header = Element.el [ alignLeft, Font.bold ] (text "Key")
            , view = \record -> Element.map Basics.never (text record.key)
            }

        value : Element.Column { key : Identifier, creator : Participant, value : Types.Value } msg
        value =
            { header = Element.el [ alignLeft, Font.bold, width fill ] (text "Value")
            , view = \record -> Element.map Basics.never (Element.paragraph [] [ text (toString record.value) ])
            }

        creator : Element.Column { key : Identifier, creator : Participant, value : Types.Value } msg
        creator =
            { header = Element.el [ alignLeft, Font.bold ] (text "Creator")
            , view = \record -> Element.map Basics.never (text (unParticipant record.creator))
            }

        table : Element.Table { key : Identifier, creator : Participant, value : Types.Value } msg
        table =
            { data = records
            , columns = [ key, value, creator ]
            }
    in
        MappingTable.viewCustom state "Variables" table


localTypeState : MappingTable.State -> Dict String LocalTypeState -> Element Msg
localTypeState state bindings =
    let
        records : List { key : String, value : LocalTypeState }
        records =
            Dict.toList bindings
                |> List.map (\( key, value ) -> { key = key, value = value })

        key : Element.Column { key : String, value : LocalTypeState } msg
        key =
            { header = Element.el [ alignLeft, Font.bold ] (text "Key")
            , view = \record -> Element.map Basics.never (text record.key)
            }

        value : Element.Column { key : String, value : LocalTypeState } msg
        value =
            { header = Element.el [ alignLeft, Font.bold, width fill ] (text "Value")
            , view = \record -> Element.map Basics.never (Element.paragraph [] [ text (toString record.value) ])
            }

        table : Element.Table { key : String, value : LocalTypeState } msg
        table =
            { data = records
            , columns = [ key, value ]
            }
    in
        MappingTable.viewCustom state "Local Type States" table


controls : State -> Element (Result Instruction MappingTable.Msg)
controls state =
    MappingTable.toggleableCustom state "Controls" <|
        Element.column []
            [ Element.Input.button [] <|
                { onPress = Just SkipLets
                , label = text "Fast Forward to Send/Receive"
                }
            ]


type alias HasSidePane a =
    { a
        | variables : MappingTable.State
        , controls : MappingTable.State
        , channels : MappingTable.State
        , localTypeStates : MappingTable.State
    }


type alias Config msg =
    { toggle : DropDown -> MappingTable.Msg -> msg
    , instruction : Instruction -> msg
    }


discharge : (a -> c) -> (b -> c) -> Result a b -> c
discharge forLeft forRight either =
    case either of
        Err e ->
            forLeft e

        Ok v ->
            forRight v


sidepane : Config msg -> HasSidePane a -> Context -> List (Element msg)
sidepane { toggle, instruction } model context =
    [ controls model.controls
        |> Element.map (discharge instruction (toggle Controls))
    , variables model.variables context.bindings
        |> Element.map (toggle Variables)
    , channels model.channels context.channels
        |> Element.map (toggle Channels)
    , localTypeState model.localTypeStates context.localTypeStates
        |> Element.map (toggle LocalTypeStates)
    ]
