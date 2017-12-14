module Diagram exposing (..)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Types exposing (GlobalAtom, GlobalType, unIdentifier)


type alias Canvas =
    { width : Int, height : Int, headerHeight : Int }


drawSegments : List String -> GlobalType -> Canvas -> Svg msg
drawSegments markers global ({ width, height, headerHeight } as canvas) =
    let
        size =
            List.length markers

        segmentWidth =
            toFloat width / toFloat size

        segmentXs =
            List.range 0 (size - 1)
                |> List.map (\n -> (toFloat n + 0.5) * segmentWidth)

        arrowHead =
            Svg.marker [ id "arrowHead", markerWidth "10", markerHeight "10", refX "0", refY "3", orient "auto", markerUnits "strokeWidth" ]
                [ Svg.path [ d "M0,0 L0,6 L9,3 z", fill "#f00" ] []
                ]
    in
    Svg.g []
        [ Svg.defs [] [ arrowHead ]
        , drawMarkers (toFloat headerHeight) (List.map2 (,) segmentXs markers)
        , Svg.g [ class "thread-lines" ] (List.map (flip drawSegmentLine ( toFloat headerHeight + 5, toFloat height )) segmentXs)
        , drawPhantomTransactionLines (List.length global.atoms) (segmentWidth / 2) canvas
        , drawGlobalType global canvas
        ]


drawMarkers : Float -> List ( Float, String ) -> Svg msg
drawMarkers height labels =
    Svg.g [ class "markers" ] <|
        List.map (\( centerX, marker ) -> drawMarkerText marker ( centerX, height )) labels


drawMarkerText : String -> ( Float, Float ) -> Svg msg
drawMarkerText label ( centerX, centerY ) =
    Svg.text_
        [ x (toString centerX)
        , y (toString centerY)
        , textAnchor "middle"
        ]
        [ Svg.text label ]


drawSegmentLine : Float -> ( Float, Float ) -> Svg msg
drawSegmentLine xCo ( yMin, yMax ) =
    Svg.line
        [ x1 (toString xCo)
        , x2 (toString xCo)
        , y1 (toString yMin)
        , y2 (toString yMax)
        , strokeWidth "2"
        , stroke "black"
        ]
        []


drawPhantomTransactionLines : Int -> Float -> Canvas -> Svg msg
drawPhantomTransactionLines n padding { height, width, headerHeight } =
    let
        segmentHeight =
            toFloat (height - headerHeight) / toFloat n

        draw yCo =
            Svg.line
                [ x1 (toString padding)
                , x2 (toString (toFloat width - padding))
                , y1 (toString yCo)
                , y2 (toString yCo)
                , strokeWidth "1"
                , stroke "grey"
                , strokeDasharray "5, 5"
                ]
                []
    in
    List.range 1 n
        |> List.map (\x -> draw (toFloat x * segmentHeight + toFloat headerHeight))
        |> Svg.g [ class "phantom-transaction-lines" ]


index : a -> List a -> Int
index needle haystack =
    let
        go n list =
            case list of
                [] ->
                    Debug.crash "the impossible happened"

                x :: xs ->
                    if x == needle then
                        n
                    else
                        go (n + 1) xs
    in
    go 0 haystack


type Direction
    = Left
    | Right


drawGlobalType : GlobalType -> Canvas -> Svg msg
drawGlobalType ({ atoms } as global) canvas =
    let
        size =
            List.length global.parameters

        segmentWidth =
            toFloat canvas.width / toFloat size

        segmentHeight =
            toFloat (canvas.height - canvas.headerHeight) / toFloat (List.length atoms)
    in
    List.range 1 (List.length atoms)
        |> List.map (\x -> toFloat x * segmentHeight + toFloat canvas.headerHeight)
        |> List.map2 (\atom height -> drawGlobalAtom height segmentWidth atom global.parameters canvas) atoms
        |> Svg.g []


drawGlobalAtom : Float -> Float -> GlobalAtom -> List Types.Identifier -> Canvas -> Svg msg
drawGlobalAtom height segmentWidth { sender, receiver, tipe } parameters canvas =
    let
        start =
            index sender parameters

        end =
            index receiver parameters

        idName =
            toString height ++ unIdentifier sender ++ unIdentifier receiver ++ tipe
    in
    Svg.g []
        [ Svg.defs []
            [ Svg.marker [ id idName, markerWidth "10", markerHeight "10", refX "0", refY "3", markerUnits "strokeWidth", overflow "visible" ]
                [ Svg.text_ [ textAnchor "middle" ] [ Svg.text tipe ] ]
            ]
        , lineWithMidpoint
            ( (toFloat start + 0.5) * segmentWidth
            , height
            )
            ( (toFloat end + 0.5)
                * segmentWidth
                + (if start > end then
                    15
                   else
                    -15
                  )
            , height
            )
            [ strokeWidth "2"
            , stroke "red"
            , markerEnd "url(#arrowHead)"
            , markerMid ("url(#" ++ idName ++ ")")
            ]
        ]


lineWithMidpoint : ( Float, Float ) -> ( Float, Float ) -> List (Svg.Attribute msg) -> Svg msg
lineWithMidpoint (( px1, py1 ) as p1) (( px2, py2 ) as p2) attributes =
    let
        midpoint =
            ( (px1 + px2) / 2
            , (py1 + py2) / 2
            )

        toStringPoint ( x, y ) =
            toString x ++ "," ++ toString y ++ " "

        p =
            "M" ++ toStringPoint p1 ++ "L" ++ toStringPoint midpoint ++ toStringPoint p2
    in
    Svg.path (d p :: attributes) []
