module MappingTable exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import FontAwesome
import Dict exposing (Dict)
import Color
import Html.Attributes


icon awesome size =
    Element.html (awesome Color.grey size)


type State
    = Open
    | Closed


toggle : State -> State
toggle state =
    case state of
        Open ->
            Closed

        Closed ->
            Open


type Msg
    = Toggle


type alias Config k v =
    { name : String
    , viewKey : k -> Element Never
    , viewValue : k -> v -> Element Never
    }


view : State -> Config k v -> List ( k, v ) -> Element Msg
view state { name, viewKey, viewValue } data =
    let
        key : Element.Column ( k, v ) msg
        key =
            { header = Element.el [ alignLeft, Font.bold ] (text "Key")
            , view = \( k, v ) -> Element.map Basics.never (viewKey k)
            }

        value : Element.Column ( k, v ) msg
        value =
            { header = Element.el [ alignLeft, Font.bold ] (text "Value")
            , view = \( k, v ) -> Element.map Basics.never (viewValue k v)
            }

        table : Element.Table ( k, v ) msg
        table =
            { data = data
            , columns = [ key, value ]
            }
    in
        Element.column [ width fill ]
            [ Element.row []
                [ Element.el [ alignLeft ] <|
                    let
                        label =
                            case state of
                                Closed ->
                                    icon FontAwesome.caret_right 20

                                Open ->
                                    icon FontAwesome.caret_down 20
                    in
                        Element.Input.button [] { onPress = Just Toggle, label = label }
                , Element.el [ alignLeft ] (text name)
                ]
            , case ( state, data ) of
                ( Open, _ :: _ ) ->
                    Element.el [ alignLeft ] (Element.table [ padding 10, spacing 10 ] table)

                _ ->
                    Element.empty
            ]


toggleableCustom : State -> String -> Element msg -> Element (Result msg Msg)
toggleableCustom state name content =
    let
        header =
            Element.el [ alignLeft ] <|
                let
                    label =
                        Element.row [ alignLeft ]
                            [ Element.el [ alignLeft ] <|
                                case state of
                                    Closed ->
                                        icon FontAwesome.caret_right 20

                                    Open ->
                                        icon FontAwesome.caret_down 20
                            , Element.el [ alignLeft ] (text name)
                            ]
                in
                    Element.Input.button [] { onPress = Just Toggle, label = label }

        body =
            case state of
                Open ->
                    Element.el [ width fill, alignLeft ] content

                Closed ->
                    Element.empty
    in
        Element.column [ width fill, height shrink ]
            [ Element.map Ok header
            , Element.map Err body
            ]


toggleable : State -> String -> Element Msg -> Element Msg
toggleable state name content =
    Element.column [ width fill, height shrink ]
        [ Element.el [ alignLeft ] <|
            let
                label =
                    Element.row [ alignLeft ]
                        [ Element.el [ alignLeft ] <|
                            case state of
                                Closed ->
                                    icon FontAwesome.caret_right 20

                                Open ->
                                    icon FontAwesome.caret_down 20
                        , Element.el [ alignLeft ] (text name)
                        ]
            in
                Element.Input.button [] { onPress = Just Toggle, label = label }
        , case state of
            Open ->
                Element.el [ width fill, alignLeft ] content

            Closed ->
                Element.empty
        ]


viewCustom : State -> String -> Table record Msg -> Element Msg
viewCustom state name table =
    toggleable state name <|
        case table.data of
            [] ->
                Element.empty

            _ :: _ ->
                Element.table [ padding 10, spacing 10 ] table
