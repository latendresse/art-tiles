port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import File exposing (File)
import File.Download
import File.Select
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
import Time



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


{-| JS side writes the string to localStorage under a well-known key.
-}
port persistState : String -> Cmd msg



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
        |> List.map (\( x, y ) -> ( p.col + x * p.scale, p.row + y * p.scale ))


placedLetterPos : PlacedTile -> TileSpec -> ( Float, Float )
placedLetterPos p spec =
    let
        ( x, y ) =
            rotatePoint p.rotation (specDims spec) spec.letterPos
    in
    ( p.col + x * p.scale, p.row + y * p.scale )


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
                    |> List.map (\( x, y ) -> ( p.col + x * p.scale, p.row + y * p.scale ))
            )



-- ============================ Overlap ============================


{-| Cells occupied by this tile, only meaningful for native-scale tiles.
Deflated tiles (scale != 1) are ignored in overlap detection.
-}
tileCells : PlacedTile -> Set ( Int, Int )
tileCells p =
    if p.scale /= 1.0 then
        Set.empty

    else
        case lookupSpec p.kind of
            Just spec ->
                specCellsRotated p.rotation spec
                    |> List.map (\( c, r ) -> ( c + round p.col, r + round p.row ))
                    |> Set.fromList

            Nothing ->
                Set.empty


{-| Compute the set of world cells covered by at least two different
clusters — i.e. true cell-level overlap, ignoring bounding boxes. Used
to visually flag bad expansions so the user can move clusters apart.
-}
computeOverlapCells : List PlacedTile -> Set ( Int, Int )
computeOverlapCells placed =
    let
        -- Group tiles by cluster id; each group's union-of-cells is one
        -- cluster's footprint.
        clusterFootprints =
            placed
                |> List.foldl
                    (\t acc ->
                        Dict.update t.clusterId
                            (\m ->
                                let
                                    cells =
                                        tileCells t
                                in
                                Just (Set.union cells (Maybe.withDefault Set.empty m))
                            )
                            acc
                    )
                    Dict.empty
                |> Dict.values

        intersections =
            pairs clusterFootprints
                |> List.map (\( a, b ) -> Set.intersect a b)
    in
    List.foldl Set.union Set.empty intersections


pairs : List a -> List ( a, a )
pairs xs =
    case xs of
        [] ->
            []

        x :: rest ->
            List.map (\y -> ( x, y )) rest ++ pairs rest


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



-- ============================ Bounding box / fit-to-view ============================


type alias BBox =
    { x1 : Float, y1 : Float, x2 : Float, y2 : Float }


{-| World-space bounding box of a single placed tile, accounting for scale
and whether rotation has swapped its native width/height.
-}
tileBounds : PlacedTile -> Maybe BBox
tileBounds t =
    case lookupSpec t.kind of
        Just spec ->
            let
                ( h, w ) =
                    specDims spec

                ( sw, sh ) =
                    if modBy 2 t.rotation == 0 then
                        ( toFloat w, toFloat h )

                    else
                        ( toFloat h, toFloat w )
            in
            Just
                { x1 = t.col
                , y1 = t.row
                , x2 = t.col + sw * t.scale
                , y2 = t.row + sh * t.scale
                }

        Nothing ->
            Nothing


tilesBoundingBox : List PlacedTile -> Maybe BBox
tilesBoundingBox tiles =
    tiles
        |> List.filterMap tileBounds
        |> List.foldl
            (\b acc ->
                case acc of
                    Nothing ->
                        Just b

                    Just a ->
                        Just
                            { x1 = min a.x1 b.x1
                            , y1 = min a.y1 b.y1
                            , x2 = max a.x2 b.x2
                            , y2 = max a.y2 b.y2
                            }
            )
            Nothing


{-| Pan so the tiling's bounding box is centered in the viewport, without
changing the zoom level.
-}
recenterOnTiles : Model -> Model
recenterOnTiles model =
    case tilesBoundingBox model.placed of
        Just bbox ->
            let
                vpW =
                    toFloat (model.windowW * 4 // 5)

                vpH =
                    toFloat model.windowH

                viewCols =
                    vpW / toFloat model.u

                viewRows =
                    vpH / toFloat model.u

                centerX =
                    (bbox.x1 + bbox.x2) / 2

                centerY =
                    (bbox.y1 + bbox.y2) / 2
            in
            { model
                | panX = centerX - viewCols / 2
                , panY = centerY - viewRows / 2
            }

        Nothing ->
            model


{-| Choose a zoom level and pan so the tiling's bounding box fits in the
current viewport, with a small margin, and is centered. May zoom in or out.
-}
fitToView : Model -> Model
fitToView model =
    case tilesBoundingBox model.placed of
        Just bbox ->
            let
                bboxW =
                    max 1.0 (bbox.x2 - bbox.x1)

                bboxH =
                    max 1.0 (bbox.y2 - bbox.y1)

                vpW =
                    toFloat (model.windowW * 4 // 5)

                vpH =
                    toFloat model.windowH

                fitW =
                    floor (0.9 * vpW / bboxW)

                fitH =
                    floor (0.9 * vpH / bboxH)

                newU =
                    clamp 1 maxU (min fitW fitH)

                centerX =
                    (bbox.x1 + bbox.x2) / 2

                centerY =
                    (bbox.y1 + bbox.y2) / 2

                viewCols =
                    vpW / toFloat newU

                viewRows =
                    vpH / toFloat newU
            in
            { model
                | u = newU
                , panX = centerX - viewCols / 2
                , panY = centerY - viewRows / 2
            }

        Nothing ->
            model



-- ============================ Substitution ============================


type alias ChildTile =
    { kind : String
    , col : Int
    , row : Int
    , rotation : Int
    }


type alias SubRule =
    { children : List ChildTile }


{-| Turn the currently placed tiles into a SubRule, normalised so the
bounding-box top-left is at (0, 0).
-}
captureRuleFromPlaced : List PlacedTile -> SubRule
captureRuleFromPlaced placed =
    let
        minC =
            placed |> List.map .col |> List.minimum |> Maybe.withDefault 0

        minR =
            placed |> List.map .row |> List.minimum |> Maybe.withDefault 0
    in
    { children =
        placed
            |> List.map
                (\t ->
                    { kind = t.kind
                    , col = round (t.col - minC)
                    , row = round (t.row - minR)
                    , rotation = t.rotation
                    }
                )
    }


{-| Native (effective) width/height of a tile kind at a given rotation.
Rotations 1 and 3 swap width and height.
-}
effectiveDims : Int -> Int -> Int -> ( Int, Int )
effectiveDims w h rot =
    if modBy 2 rot == 0 then
        ( w, h )

    else
        ( h, w )


{-| Apply a parent rotation to a child's position and rotation, interpreting
the child as living in a `ruleW × ruleH` rule box.
-}
rotateChild : Int -> Int -> Int -> ChildTile -> ChildTile
rotateChild parentRot ruleW ruleH c =
    let
        ( nativeH, nativeW ) =
            case lookupSpec c.kind of
                Just spec ->
                    specDims spec

                Nothing ->
                    ( 8, 8 )

        ( cw, ch ) =
            effectiveDims nativeW nativeH c.rotation

        ( newCol, newRow ) =
            case modBy 4 parentRot of
                0 ->
                    ( c.col, c.row )

                1 ->
                    ( ruleH - c.row - ch, c.col )

                2 ->
                    ( ruleW - c.col - cw, ruleH - c.row - ch )

                _ ->
                    ( c.row, ruleW - c.col - cw )
    in
    { kind = c.kind
    , col = newCol
    , row = newRow
    , rotation = modBy 4 (c.rotation + parentRot)
    }


{-| Rule box dimensions for a parent tile kind at rotation 0:
factor × the parent's native (W, H).
-}
ruleBoxDims : Int -> String -> ( Int, Int )
ruleBoxDims factor kind =
    case lookupSpec kind of
        Just spec ->
            let
                ( h, w ) =
                    specDims spec
            in
            ( factor * w, factor * h )

        Nothing ->
            ( factor * 8, factor * 8 )


{-| Inflation substitution: parent position scales by `factor`; each child
lands at the rotated position within the parent's inflated footprint. The
child's rotation gains the parent's rotation. All produced children share
one clusterId so they can be moved together.
-}
expandTile : Int -> Dict String SubRule -> Int -> PlacedTile -> List PlacedTile
expandTile clusterId rules factor t =
    let
        kf =
            toFloat factor
    in
    case Dict.get t.kind rules of
        Just rule ->
            let
                ( ruleW, ruleH ) =
                    ruleBoxDims factor t.kind
            in
            rule.children
                |> List.map (rotateChild t.rotation ruleW ruleH)
                |> List.map
                    (\c ->
                        { id = 0
                        , kind = c.kind
                        , col = t.col * kf + toFloat c.col * t.scale
                        , row = t.row * kf + toFloat c.row * t.scale
                        , rotation = c.rotation
                        , scale = t.scale
                        , clusterId = clusterId
                        }
                    )

        Nothing ->
            [ { t | col = t.col * kf, row = t.row * kf, clusterId = clusterId } ]


{-| Expand one parent into a cluster at native scale, positioned at the
rule's local origin (i.e. ignoring the parent's world position). If the
parent has no rule, the cluster is just the parent itself at origin.
-}
expandToCluster : Int -> Dict String SubRule -> Int -> PlacedTile -> List PlacedTile
expandToCluster clusterId rules factor t =
    case Dict.get t.kind rules of
        Just rule ->
            let
                ( ruleW, ruleH ) =
                    ruleBoxDims factor t.kind
            in
            rule.children
                |> List.map (rotateChild t.rotation ruleW ruleH)
                |> List.map
                    (\c ->
                        { id = 0
                        , kind = c.kind
                        , col = toFloat c.col
                        , row = toFloat c.row
                        , rotation = c.rotation
                        , scale = t.scale
                        , clusterId = clusterId
                        }
                    )

        Nothing ->
            [ { t | col = 0, row = 0, clusterId = clusterId } ]


bboxesOverlap : BBox -> BBox -> Bool
bboxesOverlap a b =
    a.x1 < b.x2 && a.x2 > b.x1 && a.y1 < b.y2 && a.y2 > b.y1


{-| Minimum translation vector to separate bbox `b` from bbox `a`.
Pushes `b` along whichever axis has the smaller overlap, away from `a`'s centre.
Returns (0, 0) if the boxes don't overlap.
-}
mtvToSeparate : BBox -> BBox -> ( Float, Float )
mtvToSeparate a b =
    let
        overlapX =
            min a.x2 b.x2 - max a.x1 b.x1

        overlapY =
            min a.y2 b.y2 - max a.y1 b.y1
    in
    if overlapX <= 0 || overlapY <= 0 then
        ( 0, 0 )

    else
        let
            -- Push by overlap + a generous buffer so bboxes end up with a
            -- visible gap rather than just barely touching.
            buffer =
                2.0
        in
        if overlapX <= overlapY then
            let
                bCenter =
                    (b.x1 + b.x2) / 2

                aCenter =
                    (a.x1 + a.x2) / 2
            in
            if bCenter >= aCenter then
                ( overlapX + buffer, 0 )

            else
                ( -(overlapX + buffer), 0 )

        else
            let
                bCenter =
                    (b.y1 + b.y2) / 2

                aCenter =
                    (a.y1 + a.y2) / 2
            in
            if bCenter >= aCenter then
                ( 0, overlapY + buffer )

            else
                ( 0, -(overlapY + buffer) )


shiftCluster : ( Float, Float ) -> List PlacedTile -> List PlacedTile
shiftCluster ( dx, dy ) cluster =
    cluster |> List.map (\t -> { t | col = t.col + dx, row = t.row + dy })


{-| Repeatedly find the first overlapping pair and push the later one along
its minimum translation axis, until no cluster pair overlaps or an iteration
cap is hit.
-}
resolveClusterOverlaps : List (List PlacedTile) -> List (List PlacedTile)
resolveClusterOverlaps initial =
    let
        maxIterations =
            500

        loop : Int -> List (List PlacedTile) -> List (List PlacedTile)
        loop n clusters =
            if n <= 0 then
                clusters

            else
                case firstOverlappingPair clusters of
                    Nothing ->
                        clusters

                    Just ( iEarly, jLater, push ) ->
                        let
                            _ =
                                ( iEarly, jLater )
                        in
                        loop (n - 1)
                            (List.indexedMap
                                (\k c ->
                                    if k == jLater then
                                        shiftCluster push c

                                    else
                                        c
                                )
                                clusters
                            )
    in
    loop maxIterations initial


{-| Find the first (by scan order) pair of clusters whose bounding boxes
overlap. Returns the two indices and the MTV that moves the later one
away from the earlier one.
-}
firstOverlappingPair : List (List PlacedTile) -> Maybe ( Int, Int, ( Float, Float ) )
firstOverlappingPair clusters =
    let
        bboxes : List (Maybe BBox)
        bboxes =
            List.map tilesBoundingBox clusters

        indexed : List ( Int, Maybe BBox )
        indexed =
            List.indexedMap Tuple.pair bboxes

        findAgainst : Int -> BBox -> List ( Int, Maybe BBox ) -> Maybe ( Int, ( Float, Float ) )
        findAgainst iEarly earlier rest =
            case rest of
                [] ->
                    Nothing

                ( jLater, maybeBB ) :: more ->
                    case maybeBB of
                        Just bb ->
                            if bboxesOverlap earlier bb then
                                Just ( jLater, mtvToSeparate earlier bb )

                            else
                                findAgainst iEarly earlier more

                        Nothing ->
                            findAgainst iEarly earlier more

        scan : List ( Int, Maybe BBox ) -> Maybe ( Int, Int, ( Float, Float ) )
        scan items =
            case items of
                [] ->
                    Nothing

                ( iEarly, maybeBB ) :: rest ->
                    case maybeBB of
                        Just earlier ->
                            case findAgainst iEarly earlier rest of
                                Just ( jLater, vec ) ->
                                    Just ( iEarly, jLater, vec )

                                Nothing ->
                                    scan rest

                        Nothing ->
                            scan rest
    in
    scan indexed


{-| Lay clusters out in a horizontal row, starting at x=0, each cluster
shifted so its left edge sits at the accumulated x position, with `spacing`
native cells of gap between them.
-}
layoutClustersInRow : Float -> List (List PlacedTile) -> List PlacedTile
layoutClustersInRow spacing clusters =
    let
        ( _, collected ) =
            List.foldl
                (\cluster ( xPos, acc ) ->
                    case tilesBoundingBox cluster of
                        Just bbox ->
                            let
                                dx =
                                    xPos - bbox.x1

                                shifted =
                                    cluster |> List.map (\t -> { t | col = t.col + dx })

                                w =
                                    bbox.x2 - bbox.x1
                            in
                            ( xPos + w + spacing, acc ++ shifted )

                        Nothing ->
                            ( xPos, acc )
                )
                ( 0, [] )
                clusters
    in
    collected


{-| Deflation substitution: children fit inside the parent's footprint at
scale / factor, so the replacement does not overlap the parent's neighbours.
Parent rotation is also applied. All produced children share one clusterId.
-}
deflateTile : Int -> Dict String SubRule -> Int -> PlacedTile -> List PlacedTile
deflateTile clusterId rules factor t =
    case Dict.get t.kind rules of
        Just rule ->
            let
                childScale =
                    t.scale / toFloat factor

                ( ruleW, ruleH ) =
                    ruleBoxDims factor t.kind
            in
            rule.children
                |> List.map (rotateChild t.rotation ruleW ruleH)
                |> List.map
                    (\c ->
                        { id = 0
                        , kind = c.kind
                        , col = t.col + toFloat c.col * childScale
                        , row = t.row + toFloat c.row * childScale
                        , rotation = c.rotation
                        , scale = childScale
                        , clusterId = clusterId
                        }
                    )

        Nothing ->
            [ { t | clusterId = clusterId } ]


renumber : List PlacedTile -> ( List PlacedTile, Int )
renumber tiles =
    ( List.indexedMap (\i t -> { t | id = i }) tiles
    , List.length tiles
    )



-- ============================ Save / serialize ============================


encodeTile : PlacedTile -> E.Value
encodeTile p =
    E.object
        [ ( "kind", E.string p.kind )
        , ( "col", E.float p.col )
        , ( "row", E.float p.row )
        , ( "rotation", E.int p.rotation )
        , ( "scale", E.float p.scale )
        , ( "clusterId", E.int p.clusterId )
        ]


encodeChildTile : ChildTile -> E.Value
encodeChildTile c =
    E.object
        [ ( "kind", E.string c.kind )
        , ( "col", E.int c.col )
        , ( "row", E.int c.row )
        , ( "rotation", E.int c.rotation )
        ]


encodeSubRule : SubRule -> E.Value
encodeSubRule rule =
    E.object [ ( "children", E.list encodeChildTile rule.children ) ]


encodeRules : Dict String SubRule -> E.Value
encodeRules rules =
    rules
        |> Dict.toList
        |> List.map (\( k, r ) -> ( k, encodeSubRule r ))
        |> E.object


encodeTiling : Model -> String
encodeTiling model =
    E.encode 2
        (E.object
            [ ( "version", E.int 2 )
            , ( "tiles", E.list encodeTile model.placed )
            , ( "rules", encodeRules model.rules )
            , ( "factor", E.int model.factor )
            ]
        )


type alias SavedTile =
    { kind : String
    , col : Float
    , row : Float
    , rotation : Int
    , scale : Float
    , clusterId : Maybe Int
    }


decodeSavedTile : D.Decoder SavedTile
decodeSavedTile =
    D.map6 SavedTile
        (D.field "kind" D.string)
        (D.field "col" D.float)
        (D.field "row" D.float)
        (D.field "rotation" D.int)
        (D.oneOf [ D.field "scale" D.float, D.succeed 1.0 ])
        (D.oneOf [ D.field "clusterId" D.int |> D.map Just, D.succeed Nothing ])


decodeChildTile : D.Decoder ChildTile
decodeChildTile =
    D.map4 ChildTile
        (D.field "kind" D.string)
        (D.field "col" D.int)
        (D.field "row" D.int)
        (D.field "rotation" D.int)


decodeSubRule : D.Decoder SubRule
decodeSubRule =
    D.map SubRule (D.field "children" (D.list decodeChildTile))


decodeRules : D.Decoder (Dict String SubRule)
decodeRules =
    D.dict decodeSubRule


type alias SavedTiling =
    { tiles : List SavedTile
    , rules : Dict String SubRule
    , factor : Int
    }


decodeTiling : D.Decoder SavedTiling
decodeTiling =
    D.map3 SavedTiling
        (D.field "tiles" (D.list decodeSavedTile))
        (D.oneOf
            [ D.field "rules" decodeRules
            , D.succeed Dict.empty
            ]
        )
        (D.oneOf
            [ D.field "factor" D.int
            , D.succeed 2
            ]
        )


savedToPlaced : Int -> SavedTile -> PlacedTile
savedToPlaced id s =
    { id = id
    , kind = s.kind
    , col = s.col
    , row = s.row
    , rotation = s.rotation
    , scale = s.scale

    -- Legacy files may not carry clusterId; give each such tile its own
    -- singleton cluster so they don't all drag together.
    , clusterId = Maybe.withDefault id s.clusterId
    }


monthNum : Time.Month -> Int
monthNum m =
    case m of
        Time.Jan -> 1
        Time.Feb -> 2
        Time.Mar -> 3
        Time.Apr -> 4
        Time.May -> 5
        Time.Jun -> 6
        Time.Jul -> 7
        Time.Aug -> 8
        Time.Sep -> 9
        Time.Oct -> 10
        Time.Nov -> 11
        Time.Dec -> 12


timestampName : Time.Zone -> Time.Posix -> String
timestampName zone posix =
    let
        pad2 =
            String.padLeft 2 '0' << String.fromInt

        pad4 =
            String.padLeft 4 '0' << String.fromInt
    in
    "tiling-"
        ++ pad4 (Time.toYear zone posix)
        ++ pad2 (monthNum (Time.toMonth zone posix))
        ++ pad2 (Time.toDay zone posix)
        ++ "-"
        ++ pad2 (Time.toHour zone posix)
        ++ pad2 (Time.toMinute zone posix)
        ++ pad2 (Time.toSecond zone posix)
        ++ ".json"


saveCmd : Time.Zone -> Time.Posix -> Model -> Cmd Msg
saveCmd zone posix model =
    File.Download.string (timestampName zone posix)
        "application/json"
        (encodeTiling model)



-- ============================ Model ============================


type alias PlacedTile =
    { id : Int
    , kind : String
    , col : Float
    , row : Float
    , rotation : Int
    , scale : Float
    , clusterId : Int
    }


type alias TileDrag =
    { clusterId : Int
    , origPositions : Dict Int ( Float, Float )
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
    , nextClusterId : Int
    , selectedKind : Maybe String
    , selectedPlaced : Maybe Int
    , rotation : Int
    , drag : Maybe DragState
    , panX : Float
    , panY : Float
    , u : Int
    , windowW : Int
    , windowH : Int
    , rules : Dict String SubRule
    , factor : Int
    }


type alias Flags =
    String


encodePersistedState : Dict String SubRule -> Int -> String
encodePersistedState rules factor =
    E.encode 0
        (E.object
            [ ( "rules", encodeRules rules )
            , ( "factor", E.int factor )
            ]
        )


decodePersistedState : String -> ( Dict String SubRule, Int )
decodePersistedState raw =
    let
        decoder =
            D.map2 Tuple.pair
                (D.oneOf [ D.field "rules" decodeRules, D.succeed Dict.empty ])
                (D.oneOf [ D.field "factor" D.int, D.succeed 2 ])
    in
    case D.decodeString decoder raw of
        Ok pair ->
            pair

        Err _ ->
            ( Dict.empty, 2 )


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( rules, factor ) =
            decodePersistedState flags
    in
    ( { placed = []
      , nextId = 0
      , nextClusterId = 0
      , selectedKind = Nothing
      , selectedPlaced = Nothing
      , rotation = 0
      , drag = Nothing
      , panX = 0
      , panY = 0
      , u = defaultU
      , windowW = 1200
      , windowH = 800
      , rules = rules
      , factor = factor
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
    | FitView
    | SaveMsg
    | SaveAtTime Time.Zone Time.Posix
    | LoadMsg
    | LoadFileSelected File
    | LoadFileLoaded String
    | CaptureRule String
    | ShowRule String
    | ApplyAll
    | ApplySelected



-- ============================ Update ============================


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, baseCmd ) =
            baseUpdate msg model

        persistCmd =
            if newModel.rules /= model.rules || newModel.factor /= model.factor then
                persistState (encodePersistedState newModel.rules newModel.factor)

            else
                Cmd.none
    in
    ( newModel, Cmd.batch [ baseCmd, persistCmd ] )


baseUpdate : Msg -> Model -> ( Model, Cmd Msg )
baseUpdate msg model =
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
                            , col = toFloat worldCol
                            , row = toFloat worldRow
                            , rotation = model.rotation
                            , scale = 1.0
                            , clusterId = model.nextClusterId
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
                            , nextClusterId = model.nextClusterId + 1
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
                    let
                        -- Snapshot the positions of every tile in the same
                        -- cluster so we can shift them together.
                        origPositions =
                            model.placed
                                |> List.filter (\t -> t.clusterId == p.clusterId)
                                |> List.map (\t -> ( t.id, ( t.col, t.row ) ))
                                |> Dict.fromList
                    in
                    { model
                        | drag =
                            Just
                                (DraggingTile
                                    { clusterId = p.clusterId
                                    , origPositions = origPositions
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

                        dColF =
                            toFloat dCol

                        dRowF =
                            toFloat dRow

                        -- Shift every cluster member by the same delta from
                        -- its snapshot position. Tiles outside the cluster
                        -- are left alone. No overlap check on cluster drag
                        -- (neighbours of a cluster are expected to coexist).
                        newPlaced =
                            model.placed
                                |> List.map
                                    (\t ->
                                        case Dict.get t.id state.origPositions of
                                            Just ( oc, or_ ) ->
                                                { t | col = oc + dColF, row = or_ + dRowF }

                                            Nothing ->
                                                t
                                    )
                    in
                    { model | placed = newPlaced }

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

        FitView ->
            fitToView model

        SaveMsg ->
            model

        SaveAtTime _ _ ->
            model

        LoadMsg ->
            model

        LoadFileSelected _ ->
            model

        LoadFileLoaded content ->
            case D.decodeString decodeTiling content of
                Ok saved ->
                    let
                        startId =
                            model.nextId

                        newTiles =
                            List.indexedMap
                                (\i s -> savedToPlaced (startId + i) s)
                                saved.tiles

                        -- Ensure nextClusterId is past every cluster id we just loaded.
                        maxCid =
                            newTiles
                                |> List.map .clusterId
                                |> List.maximum
                                |> Maybe.withDefault -1

                        mergedRules =
                            Dict.union saved.rules model.rules
                    in
                    { model
                        | placed = newTiles
                        , nextId = startId + List.length saved.tiles
                        , nextClusterId = max model.nextClusterId (maxCid + 1)
                        , selectedPlaced = Nothing
                        , selectedKind = Nothing
                        , drag = Nothing
                        , panX = 0
                        , panY = 0
                        , rules = mergedRules
                        , factor = saved.factor
                    }

                Err _ ->
                    model

        CaptureRule kind ->
            if List.isEmpty model.placed then
                model

            else
                { model
                    | rules =
                        Dict.insert kind
                            (captureRuleFromPlaced model.placed)
                            model.rules
                }

        ShowRule kind ->
            case Dict.get kind model.rules of
                Just rule ->
                    let
                        startId =
                            model.nextId

                        cid =
                            model.nextClusterId

                        newTiles =
                            rule.children
                                |> List.indexedMap
                                    (\i c ->
                                        { id = startId + i
                                        , kind = c.kind
                                        , col = toFloat c.col
                                        , row = toFloat c.row
                                        , rotation = c.rotation
                                        , scale = 1.0
                                        , clusterId = cid
                                        }
                                    )
                    in
                    { model
                        | placed = newTiles
                        , nextId = startId + List.length newTiles
                        , nextClusterId = cid + 1
                        , selectedPlaced = Nothing
                        , selectedKind = Nothing
                        , drag = Nothing
                        , panX = 0
                        , panY = 0
                    }

                Nothing ->
                    { model
                        | placed = []
                        , selectedPlaced = Nothing
                        , selectedKind = Nothing
                        , drag = Nothing
                    }

        ApplyAll ->
            let
                -- Each parent becomes one cluster with a fresh clusterId.
                ( clusters, nextCid ) =
                    model.placed
                        |> List.foldl
                            (\parent ( acc, cid ) ->
                                ( acc ++ [ expandTile cid model.rules model.factor parent ]
                                , cid + 1
                                )
                            )
                            ( [], model.nextClusterId )

                -- Use cluster bounding boxes to push overlapping clusters
                -- apart. The user then drags each cluster back to its
                -- interlocking position.
                resolved =
                    resolveClusterOverlaps clusters

                newTiles =
                    List.concat resolved

                ( withIds, count ) =
                    renumber newTiles
            in
            recenterOnTiles
                { model
                    | placed = withIds
                    , nextId = count
                    , nextClusterId = nextCid
                    , selectedPlaced = Nothing
                    , selectedKind = Nothing
                }

        ApplySelected ->
            case model.selectedPlaced of
                Nothing ->
                    model

                Just sid ->
                    case model.placed |> List.filter (\t -> t.id == sid) |> List.head of
                        Just tile ->
                            let
                                cid =
                                    model.nextClusterId

                                children =
                                    deflateTile cid model.rules model.factor tile

                                others =
                                    model.placed |> List.filter (\t -> t.id /= sid)

                                ( withIds, count ) =
                                    renumber (others ++ children)
                            in
                            { model
                                | placed = withIds
                                , nextId = count
                                , nextClusterId = cid + 1
                                , selectedPlaced = Nothing
                                , selectedKind = Nothing
                            }

                        Nothing ->
                            model
    , case msg of
        SaveMsg ->
            Task.perform identity (Task.map2 SaveAtTime Time.here Time.now)

        SaveAtTime zone posix ->
            saveCmd zone posix model

        LoadMsg ->
            File.Select.file [ "application/json" ] LoadFileSelected

        LoadFileSelected file ->
            Task.perform LoadFileLoaded (File.toString file)

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
        uf =
            toFloat u_

        cellSz =
            p.scale * uf

        localCells =
            specCellsRotated p.rotation spec

        -- Screen top-left (px) for a local tile cell (lc, lr).
        cellPx ( lc, lr ) =
            ( (p.col + toFloat lc * p.scale) * uf
            , (p.row + toFloat lr * p.scale) * uf
            )

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

        cellRect lc =
            let
                ( px, py ) =
                    cellPx lc
            in
            rect
                ([ SA.x (String.fromFloat px)
                 , SA.y (String.fromFloat py)
                 , SA.width (String.fromFloat cellSz)
                 , SA.height (String.fromFloat cellSz)
                 , SA.fill spec.color
                 ]
                    ++ interaction
                )
                []

        clipRect lc =
            let
                ( px, py ) =
                    cellPx lc
            in
            rect
                [ SA.x (String.fromFloat px)
                , SA.y (String.fromFloat py)
                , SA.width (String.fromFloat cellSz)
                , SA.height (String.fromFloat cellSz)
                ]
                []

        clipDef =
            defs []
                [ Svg.clipPath [ SA.id clipId ]
                    (List.map clipRect localCells)
                ]

        pointsAttr path =
            path
                |> List.map
                    (\( x, y ) ->
                        String.fromFloat (x * uf)
                            ++ ","
                            ++ String.fromFloat (y * uf)
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
                    , SA.strokeWidth (String.fromFloat cellSz)
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
                            , SA.strokeWidth (String.fromFloat (cellSz / 5))
                            , SA.fill "none"
                            , SA.strokeLinecap "butt"
                            , SA.clipPath ("url(#" ++ clipId ++ ")")
                            , SA.pointerEvents "none"
                            ]
                            []
                    )

        selection =
            if isSelected then
                localCells
                    |> List.map
                        (\lc ->
                            let
                                ( px, py ) =
                                    cellPx lc
                            in
                            rect
                                [ SA.x (String.fromFloat px)
                                , SA.y (String.fromFloat py)
                                , SA.width (String.fromFloat cellSz)
                                , SA.height (String.fromFloat cellSz)
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
                    lx * uf

                lyPx =
                    ly * uf

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
                , SA.fontSize (String.fromFloat (cellSz * 1.0))
                , SA.fontFamily "Georgia, serif"
                , SA.fontStyle "italic"
                , SA.fontWeight "bold"
                , SA.fill spec.color
                , SA.pointerEvents "none"
                , SA.transform rotTransform
                ]
                [ Svg.text spec.name ]
    in
    clipDef :: List.map cellRect localCells ++ bandList ++ markerList ++ selection ++ [ letter ]



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
            , button [ HE.onClick FitView ] [ text "Fit" ]
            , button [ HE.onClick ResetView ] [ text "Reset" ]
            ]
        , h3 [] [ text "Substitution" ]
        , div [ HA.class "controls" ]
            [ button [ HE.onClick (CaptureRule "A") ] [ text "Capture A" ]
            , button [ HE.onClick (CaptureRule "R") ] [ text "Capture R" ]
            , button [ HE.onClick (CaptureRule "T") ] [ text "Capture T" ]
            , button [ HE.onClick (ShowRule "A") ] [ text "Show A" ]
            , button [ HE.onClick (ShowRule "R") ] [ text "Show R" ]
            , button [ HE.onClick (ShowRule "T") ] [ text "Show T" ]
            , button [ HE.onClick ApplyAll ] [ text "Apply all" ]
            , button [ HE.onClick ApplySelected ] [ text "Apply selected" ]
            ]
        , viewRulesStatus model.rules
        , h3 [] [ text "File" ]
        , div [ HA.class "controls" ]
            [ button [ HE.onClick SaveMsg ] [ text "Save" ]
            , button [ HE.onClick LoadMsg ] [ text "Load" ]
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


viewRulesStatus : Dict String SubRule -> Html Msg
viewRulesStatus rules =
    let
        row kind =
            p [ HA.class "status" ]
                [ text (kind ++ ": " ++ ruleSummary rules kind) ]
    in
    div []
        [ row "A"
        , row "R"
        , row "T"
        ]


ruleSummary : Dict String SubRule -> String -> String
ruleSummary rules kind =
    case Dict.get kind rules of
        Just rule ->
            String.fromInt (List.length rule.children) ++ " children"

        Nothing ->
            "—"


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
                { id = -1, kind = spec.name, col = 0.0, row = 0.0, rotation = 0, scale = 1.0, clusterId = -1 }
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


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }
