module MappingTable exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import FontAwesome
import Dict exposing (Dict)
import Color


icon awesome size =
    Element.html (awesome Color.grey size)


type State
    = Open
    | Closed


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
            { header = Element.el [ Font.bold ] (text "Key")
            , view = \( k, v ) -> Element.map Basics.never (viewKey k)
            }

        value : Element.Column ( k, v ) msg
        value =
            { header = Element.el [ Font.bold ] (text "Value")
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
            , Element.el [] (Element.table [] table)
            ]
