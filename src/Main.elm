module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, button, div, h3, p, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Set exposing (Set)
import Svg exposing (Svg, line, rect, svg, text_)
import Svg.Attributes as SA
import Svg.Events as SE



-- ============================ Config ============================


{-| screen pixels per one grid pixel
-}
u : Int
u =
    22


boardCols : Int
boardCols =
    34


boardRows : Int
boardRows =
    22



-- ============================ Tile specs ============================


type alias TileSpec =
    { name : String
    , color : String
    , grid : List String
    , bandRows : ( Int, Int )
    }


tileA : TileSpec
tileA =
    { name = "A"
    , color = "#a4bcd9"
    , grid =
        [ "###....."
        , "########"
        , "########"
        , "########"
        , ".######."
        , ".######."
        , ".######."
        , ".##....."
        ]
    , bandRows = ( 2, 3 )
    }


tileR : TileSpec
tileR =
    { name = "R"
    , color = "#f58686"
    , grid =
        [ "...####."
        , ".######."
        , ".######."
        , ".######."
        , "########"
        , "########"
        , "######.."
        , "...###.."
        ]
    , bandRows = ( 4, 5 )
    }


tileT : TileSpec
tileT =
    { name = "T"
    , color = "#99d7a0"
    , grid =
        [ "..####"
        , "######"
        , "######"
        , ".####."
        , ".####."
        , "..###."
        ]
    , bandRows = ( 1, 2 )
    }


allSpecs : List TileSpec
allSpecs =
    [ tileA, tileR, tileT ]


lookupSpec : String -> Maybe TileSpec
lookupSpec name =
    allSpecs |> List.filter (\s -> s.name == name) |> List.head



-- ============================ Grid helpers ============================


parseGrid : List String -> List ( Int, Int )
parseGrid rows =
    rows
        |> List.indexedMap
            (\r row ->
                row
                    |> String.toList
                    |> List.indexedMap
                        (\c ch ->
                            if ch == '#' then
                                Just ( c, r )

                            else
                                Nothing
                        )
                    |> List.filterMap identity
            )
        |> List.concat


specDims : TileSpec -> ( Int, Int )
specDims spec =
    ( List.length spec.grid
    , spec.grid |> List.head |> Maybe.map String.length |> Maybe.withDefault 0
    )


{-| Rotate a cell (c, r) k quarter-turns clockwise within an H x W grid.
Returns the cell's new (col, row) in the rotated grid.
-}
rotateCell : Int -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
rotateCell k ( h, w ) ( c, r ) =
    case modBy 4 k of
        0 ->
            ( c, r )

        1 ->
            ( h - 1 - r, c )

        2 ->
            ( w - 1 - c, h - 1 - r )

        _ ->
            ( r, w - 1 - c )


specCellsRotated : Int -> TileSpec -> List ( Int, Int )
specCellsRotated k spec =
    parseGrid spec.grid |> List.map (rotateCell k (specDims spec))


specBandRotated : Int -> TileSpec -> List ( Int, Int )
specBandRotated k spec =
    let
        ( r1, r2 ) =
            spec.bandRows
    in
    parseGrid spec.grid
        |> List.filter (\( _, r ) -> r >= r1 && r <= r2)
        |> List.map (rotateCell k (specDims spec))



-- ============================ Model ============================


type alias PlacedTile =
    { id : Int
    , kind : String
    , col : Int
    , row : Int
    , rotation : Int
    }


type alias Drag =
    { id : Int
    , origCol : Int
    , origRow : Int
    , startX : Float
    , startY : Float
    }


type alias Model =
    { placed : List PlacedTile
    , nextId : Int
    , selectedKind : Maybe String
    , selectedPlaced : Maybe Int
    , rotation : Int
    , drag : Maybe Drag
    }


init : Model
init =
    { placed = []
    , nextId = 0
    , selectedKind = Nothing
    , selectedPlaced = Nothing
    , rotation = 0
    , drag = Nothing
    }



-- ============================ Msg ============================


type Msg
    = SelectKind String
    | BoardMouseDown Int Int
    | TileMouseDown Int Int Int Float Float
    | DragMouseMove Float Float
    | MouseUp
    | RotateMsg
    | DeleteMsg
    | ClearMsg



-- ============================ Update ============================


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        SelectKind n ->
            { model
                | selectedKind = Just n
                , selectedPlaced = Nothing
            }

        BoardMouseDown c r ->
            case model.selectedKind of
                Just n ->
                    { model
                        | placed =
                            model.placed
                                ++ [ { id = model.nextId
                                     , kind = n
                                     , col = c
                                     , row = r
                                     , rotation = model.rotation
                                     }
                                   ]
                        , nextId = model.nextId + 1
                        , selectedPlaced = Just model.nextId
                    }

                Nothing ->
                    { model | selectedPlaced = Nothing }

        TileMouseDown id c r cx cy ->
            case model.placed |> List.filter (\p -> p.id == id) |> List.head of
                Just p ->
                    { model
                        | drag =
                            Just
                                { id = id
                                , origCol = p.col
                                , origRow = p.row
                                , startX = cx
                                , startY = cy
                                }
                        , selectedPlaced = Just id
                        , selectedKind = Nothing
                    }

                Nothing ->
                    model

        DragMouseMove cx cy ->
            case model.drag of
                Just d ->
                    let
                        dCol =
                            round ((cx - d.startX) / toFloat u)

                        dRow =
                            round ((cy - d.startY) / toFloat u)
                    in
                    { model
                        | placed =
                            model.placed
                                |> List.map
                                    (\p ->
                                        if p.id == d.id then
                                            { p | col = d.origCol + dCol, row = d.origRow + dRow }

                                        else
                                            p
                                    )
                    }

                Nothing ->
                    model

        MouseUp ->
            { model | drag = Nothing }

        RotateMsg ->
            case model.selectedPlaced of
                Just id ->
                    { model
                        | placed =
                            model.placed
                                |> List.map
                                    (\p ->
                                        if p.id == id then
                                            { p | rotation = modBy 4 (p.rotation + 1) }

                                        else
                                            p
                                    )
                    }

                Nothing ->
                    { model | rotation = modBy 4 (model.rotation + 1) }

        DeleteMsg ->
            case model.selectedPlaced of
                Just id ->
                    { model
                        | placed = List.filter (\p -> p.id /= id) model.placed
                        , selectedPlaced = Nothing
                    }

                Nothing ->
                    model

        ClearMsg ->
            { model | placed = [], selectedPlaced = Nothing }
    , Cmd.none
    )



-- ============================ View ============================


view : Model -> Html Msg
view model =
    div [ HA.class "app" ]
        [ viewBoard model
        , viewSidebar model
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        w =
            boardCols * u

        h =
            boardRows * u
    in
    svg
        [ SA.viewBox ("0 0 " ++ String.fromInt w ++ " " ++ String.fromInt h)
        , SA.width (String.fromInt w)
        , SA.height (String.fromInt h)
        , SA.class "board"
        ]
        (background
            :: gridLines
            ++ List.concatMap (drawPlacedTileOnBoard model) model.placed
        )


background : Svg Msg
background =
    rect
        [ SA.x "0"
        , SA.y "0"
        , SA.width (String.fromInt (boardCols * u))
        , SA.height (String.fromInt (boardRows * u))
        , SA.fill "#ffffff"
        , SE.on "mousedown" (mouseDecoder BoardMouseDown)
        ]
        []


gridLines : List (Svg Msg)
gridLines =
    let
        vLine c =
            line
                [ SA.x1 (String.fromInt (c * u))
                , SA.y1 "0"
                , SA.x2 (String.fromInt (c * u))
                , SA.y2 (String.fromInt (boardRows * u))
                , SA.stroke "#e8e8e8"
                , SA.strokeWidth "1"
                , SA.pointerEvents "none"
                ]
                []

        hLine r =
            line
                [ SA.x1 "0"
                , SA.y1 (String.fromInt (r * u))
                , SA.x2 (String.fromInt (boardCols * u))
                , SA.y2 (String.fromInt (r * u))
                , SA.stroke "#e8e8e8"
                , SA.strokeWidth "1"
                , SA.pointerEvents "none"
                ]
                []
    in
    (List.range 0 boardCols |> List.map vLine)
        ++ (List.range 0 boardRows |> List.map hLine)


drawPlacedTileOnBoard : Model -> PlacedTile -> List (Svg Msg)
drawPlacedTileOnBoard model p =
    case lookupSpec p.kind of
        Just spec ->
            drawTile True (model.selectedPlaced == Just p.id) p spec

        Nothing ->
            []


drawTile : Bool -> Bool -> PlacedTile -> TileSpec -> List (Svg Msg)
drawTile onBoard isSelected p spec =
    let
        cells =
            specCellsRotated p.rotation spec
                |> List.map (\( c, r ) -> ( c + p.col, r + p.row ))

        bandSet =
            specBandRotated p.rotation spec
                |> List.map (\( c, r ) -> ( c + p.col, r + p.row ))
                |> Set.fromList

        tileHandler c r =
            SE.stopPropagationOn "mousedown"
                (D.map2
                    (\cx cy -> ( TileMouseDown p.id c r cx cy, True ))
                    (D.field "clientX" D.float)
                    (D.field "clientY" D.float)
                )

        interaction c r =
            if onBoard then
                [ SA.style "cursor:move;"
                , tileHandler c r
                ]

            else
                [ SA.pointerEvents "none" ]

        cellRect ( c, r ) =
            rect
                ([ SA.x (String.fromInt (c * u))
                 , SA.y (String.fromInt (r * u))
                 , SA.width (String.fromInt u)
                 , SA.height (String.fromInt u)
                 , SA.fill
                    (if Set.member ( c, r ) bandSet then
                        "#fff200"

                     else
                        spec.color
                    )
                 , SA.stroke "#333"
                 , SA.strokeWidth "0.6"
                 ]
                    ++ interaction c r
                )
                []

        selection =
            if isSelected then
                cells
                    |> List.map
                        (\( c, r ) ->
                            rect
                                [ SA.x (String.fromInt (c * u))
                                , SA.y (String.fromInt (r * u))
                                , SA.width (String.fromInt u)
                                , SA.height (String.fromInt u)
                                , SA.fill "none"
                                , SA.stroke "#ff6600"
                                , SA.strokeWidth "2"
                                , SA.pointerEvents "none"
                                ]
                                []
                        )

            else
                []

        letter =
            let
                band =
                    specBandRotated p.rotation spec
                        |> List.map (\( c, r ) -> ( c + p.col, r + p.row ))

                xs =
                    List.map Tuple.first band

                ys =
                    List.map Tuple.second band

                minOr0 =
                    List.minimum >> Maybe.withDefault 0

                maxOr0 =
                    List.maximum >> Maybe.withDefault 0

                cx =
                    toFloat (minOr0 xs + maxOr0 xs + 1) / 2 * toFloat u

                cy =
                    toFloat (minOr0 ys + maxOr0 ys + 1) / 2 * toFloat u
            in
            text_
                [ SA.x (String.fromFloat cx)
                , SA.y (String.fromFloat cy)
                , SA.textAnchor "middle"
                , SA.dominantBaseline "central"
                , SA.fontSize (String.fromFloat (toFloat u * 1.2))
                , SA.fontFamily "Georgia, serif"
                , SA.fontStyle "italic"
                , SA.fontWeight "bold"
                , SA.fill spec.color
                , SA.pointerEvents "none"
                ]
                [ Svg.text spec.name ]
    in
    List.map cellRect cells ++ selection ++ [ letter ]


mouseDecoder : (Int -> Int -> Msg) -> D.Decoder Msg
mouseDecoder toMsg =
    D.map2 toMsg
        (D.field "offsetX" D.float |> D.map (\x -> floor (x / toFloat u)))
        (D.field "offsetY" D.float |> D.map (\y -> floor (y / toFloat u)))


viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ HA.class "sidebar" ]
        [ h3 [] [ text "Tiles" ]
        , div [ HA.class "palette" ]
            (List.map (paletteEntry model) allSpecs)
        , h3 [] [ text "Controls" ]
        , div [ HA.class "controls" ]
            [ button [ HE.onClick RotateMsg ] [ text "Rotate" ]
            , button [ HE.onClick DeleteMsg ] [ text "Delete" ]
            , button [ HE.onClick ClearMsg ] [ text "Clear" ]
            ]
        , p [ HA.class "help" ]
            [ text "Pick a tile on the right, then click on the board to place a copy. Click a placed tile to select it (orange outline); drag it to move; Rotate turns by 90°; Delete removes it." ]
        , p [ HA.class "status" ]
            [ text
                ("Next placement rotation: "
                    ++ String.fromInt (model.rotation * 90)
                    ++ "°  ·  Placed: "
                    ++ String.fromInt (List.length model.placed)
                )
            ]
        ]


paletteEntry : Model -> TileSpec -> Html Msg
paletteEntry model spec =
    let
        isSel =
            model.selectedKind == Just spec.name

        ( h, w ) =
            specDims spec

        pu =
            14
    in
    div
        [ HA.class
            (if isSel then
                "palette-entry selected"

             else
                "palette-entry"
            )
        , HE.onClick (SelectKind spec.name)
        ]
        [ svg
            [ SA.width (String.fromInt (w * pu))
            , SA.height (String.fromInt (h * pu))
            , SA.viewBox ("0 0 " ++ String.fromInt (w * u) ++ " " ++ String.fromInt (h * u))
            , SA.style "pointer-events: none;"
            ]
            (drawTile False
                False
                { id = -1, kind = spec.name, col = 0, row = 0, rotation = 0 }
                spec
            )
        ]



-- ============================ Main ============================


subs : Model -> Sub Msg
subs model =
    case model.drag of
        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseUp (D.succeed MouseUp)
                , Browser.Events.onMouseMove
                    (D.map2 DragMouseMove
                        (D.field "clientX" D.float)
                        (D.field "clientY" D.float)
                    )
                ]

        Nothing ->
            Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subs
        }
