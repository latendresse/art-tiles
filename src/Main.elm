module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import File.Download
import Html exposing (Html, button, div, h3, p, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Svg exposing (Svg, defs, g, line, polyline, rect, svg, text_)
import Svg.Attributes as SA
import Svg.Events as SE
import Task



-- ============================ Config ============================


defaultU : Int
defaultU =
    22


minU : Int
minU =
    8


maxU : Int
maxU =
    60


zoomStep : Int
zoomStep =
    2



-- ============================ Tile specs ============================


type MarkerDir
    = H
    | V


type alias Marker =
    { col : Int
    , row : Int
    , dir : MarkerDir
    }


type alias TileSpec =
    { name : String
    , color : String
    , grid : List String
    , bandPath : List ( Float, Float )
    , letterPos : ( Float, Float )
    , markers : List Marker
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
    , bandPath = [ ( 0, 4 ), ( 3, 4 ), ( 3, 8 ) ]
    , letterPos = ( 3, 4 )
    , markers =
        [ { col = 2, row = 0, dir = H }
        , { col = 7, row = 3, dir = V }
        ]
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
    , bandPath = [ ( 3, 0 ), ( 3, 4 ), ( 8, 4 ) ]
    , letterPos = ( 3, 4 )
    , markers =
        [ { col = 0, row = 4, dir = V }
        , { col = 3, row = 7, dir = H }
        ]
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
    , bandPath = [ ( 2, 0 ), ( 2, 3 ), ( 0, 3 ) ]
    , letterPos = ( 2, 3 )
    , markers =
        [ { col = 5, row = 2, dir = V }
        , { col = 2, row = 5, dir = H }
        ]
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


rotatePoint : Int -> ( Int, Int ) -> ( Float, Float ) -> ( Float, Float )
rotatePoint k ( h, w ) ( x, y ) =
    let
        hf =
            toFloat h

        wf =
            toFloat w
    in
    case modBy 4 k of
        0 ->
            ( x, y )

        1 ->
            ( hf - y, x )

        2 ->
            ( wf - x, hf - y )

        _ ->
            ( y, wf - x )


placedBandPath : PlacedTile -> TileSpec -> List ( Float, Float )
placedBandPath p spec =
    spec.bandPath
        |> List.map (rotatePoint p.rotation (specDims spec))
        |> List.map (\( x, y ) -> ( x + toFloat p.col, y + toFloat p.row ))


placedLetterPos : PlacedTile -> TileSpec -> ( Float, Float )
placedLetterPos p spec =
    let
        ( x, y ) =
            rotatePoint p.rotation (specDims spec) spec.letterPos
    in
    ( x + toFloat p.col, y + toFloat p.row )


markerPath : Marker -> List ( Float, Float )
markerPath m =
    let
        cx =
            toFloat m.col + 0.5

        cy =
            toFloat m.row + 0.5
    in
    case m.dir of
        H ->
            [ ( cx - 0.5, cy ), ( cx + 0.5, cy ) ]

        V ->
            [ ( cx, cy - 0.5 ), ( cx, cy + 0.5 ) ]


placedMarkerPaths : PlacedTile -> TileSpec -> List (List ( Float, Float ))
placedMarkerPaths p spec =
    spec.markers
        |> List.map
            (\m ->
                markerPath m
                    |> List.map (rotatePoint p.rotation (specDims spec))
                    |> List.map (\( x, y ) -> ( x + toFloat p.col, y + toFloat p.row ))
            )



-- ============================ Overlap ============================


tileCells : PlacedTile -> Set ( Int, Int )
tileCells p =
    case lookupSpec p.kind of
        Just spec ->
            specCellsRotated p.rotation spec
                |> List.map (\( c, r ) -> ( c + p.col, r + p.row ))
                |> Set.fromList

        Nothing ->
            Set.empty


allOccupiedCells : Maybe Int -> List PlacedTile -> Set ( Int, Int )
allOccupiedCells excludeId placed =
    placed
        |> List.filter
            (\p ->
                case excludeId of
                    Just eid ->
                        p.id /= eid

                    Nothing ->
                        True
            )
        |> List.foldl (\p acc -> Set.union acc (tileCells p)) Set.empty


wouldOverlap : Set ( Int, Int ) -> PlacedTile -> Bool
wouldOverlap occupied tile =
    not (Set.isEmpty (Set.intersect occupied (tileCells tile)))



-- ============================ Save / serialize ============================


encodeTile : PlacedTile -> E.Value
encodeTile p =
    E.object
        [ ( "kind", E.string p.kind )
        , ( "col", E.int p.col )
        , ( "row", E.int p.row )
        , ( "rotation", E.int p.rotation )
        ]


encodeTiling : Model -> String
encodeTiling model =
    E.encode 2
        (E.object
            [ ( "version", E.int 1 )
            , ( "tiles", E.list encodeTile model.placed )
            ]
        )


saveCmd : Model -> Cmd Msg
saveCmd model =
    File.Download.string "tiling.json" "application/json" (encodeTiling model)



-- ============================ Model ============================


type alias PlacedTile =
    { id : Int
    , kind : String
    , col : Int
    , row : Int
    , rotation : Int
    }


type alias TileDrag =
    { id : Int
    , origCol : Int
    , origRow : Int
    , startX : Float
    , startY : Float
    }


type alias PanDragState =
    { origPanX : Float
    , origPanY : Float
    , startX : Float
    , startY : Float
    }


type DragState
    = DraggingTile TileDrag
    | DraggingPan PanDragState


type alias Model =
    { placed : List PlacedTile
    , nextId : Int
    , selectedKind : Maybe String
    , selectedPlaced : Maybe Int
    , rotation : Int
    , drag : Maybe DragState
    , panX : Float
    , panY : Float
    , u : Int
    , windowW : Int
    , windowH : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { placed = []
      , nextId = 0
      , selectedKind = Nothing
      , selectedPlaced = Nothing
      , rotation = 0
      , drag = Nothing
      , panX = 0
      , panY = 0
      , u = defaultU
      , windowW = 1200
      , windowH = 800
      }
    , Task.perform
        (\v -> Resize (round v.viewport.width) (round v.viewport.height))
        Browser.Dom.getViewport
    )


boardDims : Model -> ( Int, Int )
boardDims model =
    ( max 10 ((model.windowW * 4 // 5) // model.u)
    , max 10 (model.windowH // model.u)
    )



-- ============================ Msg ============================


type Msg
    = SelectKind String
    | BoardMouseDown Float Float Float Float
    | TileMouseDown Int Float Float
    | DragMouseMove Float Float
    | MouseUp
    | RotateMsg
    | DeleteMsg
    | ClearMsg
    | Resize Int Int
    | ZoomIn
    | ZoomOut
    | ResetView
    | SaveMsg



-- ============================ Update ============================


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        SelectKind n ->
            { model
                | selectedKind = Just n
                , selectedPlaced = Nothing
            }

        BoardMouseDown offX offY clX clY ->
            case model.selectedKind of
                Just n ->
                    let
                        worldCol =
                            floor (offX / toFloat model.u + model.panX)

                        worldRow =
                            floor (offY / toFloat model.u + model.panY)

                        newTile =
                            { id = model.nextId
                            , kind = n
                            , col = worldCol
                            , row = worldRow
                            , rotation = model.rotation
                            }

                        occupied =
                            allOccupiedCells Nothing model.placed
                    in
                    if wouldOverlap occupied newTile then
                        model

                    else
                        { model
                            | placed = model.placed ++ [ newTile ]
                            , nextId = model.nextId + 1
                            , selectedPlaced = Just model.nextId
                        }

                Nothing ->
                    { model
                        | drag =
                            Just
                                (DraggingPan
                                    { origPanX = model.panX
                                    , origPanY = model.panY
                                    , startX = clX
                                    , startY = clY
                                    }
                                )
                        , selectedPlaced = Nothing
                    }

        TileMouseDown id cx cy ->
            case model.placed |> List.filter (\p -> p.id == id) |> List.head of
                Just p ->
                    { model
                        | drag =
                            Just
                                (DraggingTile
                                    { id = id
                                    , origCol = p.col
                                    , origRow = p.row
                                    , startX = cx
                                    , startY = cy
                                    }
                                )
                        , selectedPlaced = Just id
                        , selectedKind = Nothing
                    }

                Nothing ->
                    model

        DragMouseMove cx cy ->
            case model.drag of
                Just (DraggingTile state) ->
                    let
                        dCol =
                            round ((cx - state.startX) / toFloat model.u)

                        dRow =
                            round ((cy - state.startY) / toFloat model.u)

                        newCol =
                            state.origCol + dCol

                        newRow =
                            state.origRow + dRow
                    in
                    case model.placed |> List.filter (\p -> p.id == state.id) |> List.head of
                        Just p ->
                            let
                                proposed =
                                    { p | col = newCol, row = newRow }

                                occupied =
                                    allOccupiedCells (Just state.id) model.placed
                            in
                            if wouldOverlap occupied proposed then
                                model

                            else
                                { model
                                    | placed =
                                        model.placed
                                            |> List.map
                                                (\t ->
                                                    if t.id == state.id then
                                                        proposed

                                                    else
                                                        t
                                                )
                                }

                        Nothing ->
                            model

                Just (DraggingPan state) ->
                    let
                        dx =
                            (cx - state.startX) / toFloat model.u

                        dy =
                            (cy - state.startY) / toFloat model.u
                    in
                    { model
                        | panX = state.origPanX - dx
                        , panY = state.origPanY - dy
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
                                            -- only rotate if the rotation doesn't overlap
                                            let
                                                proposed =
                                                    { p | rotation = modBy 4 (p.rotation + 1) }

                                                occupied =
                                                    allOccupiedCells (Just id) model.placed
                                            in
                                            if wouldOverlap occupied proposed then
                                                p

                                            else
                                                proposed

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

        Resize w h ->
            { model | windowW = w, windowH = h }

        ZoomIn ->
            { model | u = min maxU (model.u + zoomStep) }

        ZoomOut ->
            { model | u = max minU (model.u - zoomStep) }

        ResetView ->
            { model | u = defaultU, panX = 0, panY = 0 }

        SaveMsg ->
            model
    , case msg of
        SaveMsg ->
            saveCmd model

        _ ->
            Cmd.none
    )



-- ============================ View ============================


view : Model -> Html Msg
view model =
    div [ HA.class "app" ]
        [ div [ HA.class "board-wrap" ] [ viewBoard model ]
        , viewSidebar model
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        ( cols, rows ) =
            boardDims model

        w =
            cols * model.u

        h =
            rows * model.u

        tx =
            -model.panX * toFloat model.u

        ty =
            -model.panY * toFloat model.u

        panTransform =
            "translate("
                ++ String.fromFloat tx
                ++ ","
                ++ String.fromFloat ty
                ++ ")"
    in
    svg
        [ SA.viewBox ("0 0 " ++ String.fromInt w ++ " " ++ String.fromInt h)
        , SA.width (String.fromInt w)
        , SA.height (String.fromInt h)
        , SA.class "board"
        ]
        [ background model
        , g [ SA.transform panTransform ]
            (gridLines model
                ++ List.concatMap (drawPlacedTileOnBoard model) model.placed
            )
        ]


background : Model -> Svg Msg
background model =
    let
        ( cols, rows ) =
            boardDims model
    in
    rect
        [ SA.x "0"
        , SA.y "0"
        , SA.width (String.fromInt (cols * model.u))
        , SA.height (String.fromInt (rows * model.u))
        , SA.fill "#ffffff"
        , SE.on "mousedown" boardMouseDownDecoder
        ]
        []


boardMouseDownDecoder : D.Decoder Msg
boardMouseDownDecoder =
    D.map4 BoardMouseDown
        (D.field "offsetX" D.float)
        (D.field "offsetY" D.float)
        (D.field "clientX" D.float)
        (D.field "clientY" D.float)


gridLines : Model -> List (Svg Msg)
gridLines model =
    let
        ( cols, rows ) =
            boardDims model

        u_ =
            model.u

        minCol =
            floor model.panX - 1

        maxCol =
            minCol + cols + 3

        minRow =
            floor model.panY - 1

        maxRow =
            minRow + rows + 3

        vLine c =
            line
                [ SA.x1 (String.fromInt (c * u_))
                , SA.y1 (String.fromInt (minRow * u_))
                , SA.x2 (String.fromInt (c * u_))
                , SA.y2 (String.fromInt (maxRow * u_))
                , SA.stroke "#e8e8e8"
                , SA.strokeWidth "1"
                , SA.pointerEvents "none"
                ]
                []

        hLine r =
            line
                [ SA.x1 (String.fromInt (minCol * u_))
                , SA.y1 (String.fromInt (r * u_))
                , SA.x2 (String.fromInt (maxCol * u_))
                , SA.y2 (String.fromInt (r * u_))
                , SA.stroke "#e8e8e8"
                , SA.strokeWidth "1"
                , SA.pointerEvents "none"
                ]
                []
    in
    (List.range minCol maxCol |> List.map vLine)
        ++ (List.range minRow maxRow |> List.map hLine)


drawPlacedTileOnBoard : Model -> PlacedTile -> List (Svg Msg)
drawPlacedTileOnBoard model p =
    case lookupSpec p.kind of
        Just spec ->
            drawTile model.u True (model.selectedPlaced == Just p.id) p spec

        Nothing ->
            []


drawTile : Int -> Bool -> Bool -> PlacedTile -> TileSpec -> List (Svg Msg)
drawTile u_ onBoard isSelected p spec =
    let
        cells =
            specCellsRotated p.rotation spec
                |> List.map (\( c, r ) -> ( c + p.col, r + p.row ))

        clipId =
            "tile-clip-" ++ spec.name ++ "-" ++ String.fromInt p.id

        tileHandler =
            SE.stopPropagationOn "mousedown"
                (D.map2
                    (\cx cy -> ( TileMouseDown p.id cx cy, True ))
                    (D.field "clientX" D.float)
                    (D.field "clientY" D.float)
                )

        interaction =
            if onBoard then
                [ SA.style "cursor:move;", tileHandler ]

            else
                [ SA.pointerEvents "none" ]

        cellRect ( c, r ) =
            rect
                ([ SA.x (String.fromInt (c * u_))
                 , SA.y (String.fromInt (r * u_))
                 , SA.width (String.fromInt u_)
                 , SA.height (String.fromInt u_)
                 , SA.fill spec.color
                 ]
                    ++ interaction
                )
                []

        clipRect ( c, r ) =
            rect
                [ SA.x (String.fromInt (c * u_))
                , SA.y (String.fromInt (r * u_))
                , SA.width (String.fromInt u_)
                , SA.height (String.fromInt u_)
                ]
                []

        clipDef =
            defs []
                [ Svg.clipPath [ SA.id clipId ]
                    (List.map clipRect cells)
                ]

        pointsAttr path =
            path
                |> List.map
                    (\( x, y ) ->
                        String.fromFloat (x * toFloat u_)
                            ++ ","
                            ++ String.fromFloat (y * toFloat u_)
                    )
                |> String.join " "

        bandList =
            let
                path =
                    placedBandPath p spec
            in
            if List.length path >= 2 then
                [ polyline
                    [ SA.points (pointsAttr path)
                    , SA.stroke "#fff200"
                    , SA.strokeWidth (String.fromInt u_)
                    , SA.fill "none"
                    , SA.strokeLinejoin "miter"
                    , SA.strokeLinecap "butt"
                    , SA.clipPath ("url(#" ++ clipId ++ ")")
                    , SA.pointerEvents "none"
                    ]
                    []
                ]

            else
                []

        markerList =
            placedMarkerPaths p spec
                |> List.map
                    (\path ->
                        polyline
                            [ SA.points (pointsAttr path)
                            , SA.stroke "#fff200"
                            , SA.strokeWidth (String.fromFloat (toFloat u_ / 5))
                            , SA.fill "none"
                            , SA.strokeLinecap "butt"
                            , SA.clipPath ("url(#" ++ clipId ++ ")")
                            , SA.pointerEvents "none"
                            ]
                            []
                    )

        selection =
            if isSelected then
                cells
                    |> List.map
                        (\( c, r ) ->
                            rect
                                [ SA.x (String.fromInt (c * u_))
                                , SA.y (String.fromInt (r * u_))
                                , SA.width (String.fromInt u_)
                                , SA.height (String.fromInt u_)
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
                ( lx, ly ) =
                    placedLetterPos p spec

                lxPx =
                    lx * toFloat u_

                lyPx =
                    ly * toFloat u_

                rotTransform =
                    "rotate("
                        ++ String.fromInt (p.rotation * 90)
                        ++ " "
                        ++ String.fromFloat lxPx
                        ++ " "
                        ++ String.fromFloat lyPx
                        ++ ")"
            in
            text_
                [ SA.x (String.fromFloat lxPx)
                , SA.y (String.fromFloat lyPx)
                , SA.textAnchor "middle"
                , SA.dominantBaseline "central"
                , SA.fontSize (String.fromFloat (toFloat u_ * 1.0))
                , SA.fontFamily "Georgia, serif"
                , SA.fontStyle "italic"
                , SA.fontWeight "bold"
                , SA.fill spec.color
                , SA.pointerEvents "none"
                , SA.transform rotTransform
                ]
                [ Svg.text spec.name ]
    in
    clipDef :: List.map cellRect cells ++ bandList ++ markerList ++ selection ++ [ letter ]



-- Sidebar


viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ HA.class "sidebar" ]
        [ h3 [] [ text "Tiles" ]
        , div [ HA.class "palette" ]
            (List.map (paletteEntry model) allSpecs)
        , h3 [] [ text "Tile" ]
        , div [ HA.class "controls" ]
            [ button [ HE.onClick RotateMsg ] [ text "Rotate" ]
            , button [ HE.onClick DeleteMsg ] [ text "Delete" ]
            , button [ HE.onClick ClearMsg ] [ text "Clear" ]
            ]
        , h3 [] [ text "View" ]
        , div [ HA.class "controls" ]
            [ button [ HE.onClick ZoomIn ] [ text "Zoom +" ]
            , button [ HE.onClick ZoomOut ] [ text "Zoom \u{2212}" ]
            , button [ HE.onClick ResetView ] [ text "Reset" ]
            ]
        , h3 [] [ text "File" ]
        , div [ HA.class "controls" ]
            [ button [ HE.onClick SaveMsg ] [ text "Save" ]
            ]
        , p [ HA.class "status" ]
            [ text
                (String.fromInt (model.rotation * 90)
                    ++ "°  ·  "
                    ++ String.fromInt (List.length model.placed)
                    ++ " tiles  ·  zoom "
                    ++ String.fromInt model.u
                    ++ "px"
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
            16
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
            , SA.viewBox ("0 0 " ++ String.fromInt (w * pu) ++ " " ++ String.fromInt (h * pu))
            , SA.style "pointer-events: none; display: block;"
            ]
            (drawTile pu
                False
                False
                { id = -1, kind = spec.name, col = 0, row = 0, rotation = 0 }
                spec
            )
        ]



-- ============================ Main ============================


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Browser.Events.onResize Resize
        , case model.drag of
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
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }
