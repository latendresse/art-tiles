port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, button, div, h3, input, p, text)
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
    1


maxU : Int
maxU =
    60


{-| Zoom step depends on current zoom: 1 px at a time in the tight
range [1..10], 2 px at a time above that.
-}
zoomStep : Int -> Int
zoomStep u =
    if u <= 10 then
        1

    else
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


type alias Decoration =
    { col : Int

    -- tile-local column
    , row : Int

    -- tile-local row
    , kind : String

    -- which tile kind supplies the fill colour ("A", "R", "T")
    }


type alias TileSpec =
    { name : String
    , color : String
    , decorationColor : String

    -- saturated colour used when something else references this kind
    -- in its decoration list (see Decoration.kind).
    , grid : List String
    , bandPath : List ( Float, Float )
    , letterPos : ( Float, Float )
    , markers : List Marker
    , decorations : List Decoration
    }


tileA : TileSpec
tileA =
    { name = "A"
    , color = "#a4bcd9"
    , decorationColor = "#2e60a0"
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

    -- (row, col) input from the user translates to (col, row) here.
    , decorations =
        [ { col = 0, row = 0, kind = "R" }
        , { col = 0, row = 1, kind = "T" }
        , { col = 5, row = 1, kind = "R" }
        , { col = 6, row = 1, kind = "T" }
        , { col = 7, row = 1, kind = "R" }
        , { col = 6, row = 6, kind = "A" }
        , { col = 7, row = 6, kind = "R" }
        ]
    }


tileR : TileSpec
tileR =
    { name = "R"
    , color = "#f58686"
    , decorationColor = "#c81717"
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
    , decorations =
        [ { col = 5, row = 0, kind = "T" }
        , { col = 6, row = 0, kind = "R" }
        , { col = 5, row = 1, kind = "A" }
        , { col = 6, row = 1, kind = "A" }
        , { col = 0, row = 6, kind = "R" }
        , { col = 5, row = 6, kind = "T" }
        , { col = 5, row = 7, kind = "R" }
        ]
    }


tileT : TileSpec
tileT =
    { name = "T"
    , color = "#99d7a0"
    , decorationColor = "#1f9a35"
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
    , decorations =
        [ { col = 4, row = 0, kind = "T" }
        , { col = 5, row = 0, kind = "R" }
        , { col = 4, row = 5, kind = "R" }
        ]
    }


allSpecs : List TileSpec
allSpecs =
    [ tileA, tileR, tileT ]


lookupSpec : String -> Maybe TileSpec
lookupSpec name =
    allSpecs |> List.filter (\s -> s.name == name) |> List.head



-- ============================ Decoration colour ============================


{-| Saturated colour to use when a tile of some kind needs to paint a
decoration square referencing that kind. Each TileSpec carries its own
decorationColor so the palette stays tunable per kind.
-}
decorationColorFor : String -> String
decorationColorFor kind =
    case lookupSpec kind of
        Just spec ->
            spec.decorationColor

        Nothing ->
            "#000000"



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


{-| Set of cluster ids whose cells overlap another cluster's cells.
Scale-1 tiles only (deflated children are not included).
-}
computeOverlappingClusters : List PlacedTile -> Set Int
computeOverlappingClusters placed =
    let
        clusterFootprints : Dict Int (Set ( Int, Int ))
        clusterFootprints =
            placed
                |> List.foldl
                    (\t acc ->
                        Dict.update t.clusterId
                            (\m ->
                                Just (Set.union (tileCells t) (Maybe.withDefault Set.empty m))
                            )
                            acc
                    )
                    Dict.empty

        entries : List ( Int, Set ( Int, Int ) )
        entries =
            Dict.toList clusterFootprints
    in
    pairs entries
        |> List.concatMap
            (\( ( aId, aCells ), ( bId, bCells ) ) ->
                if Set.isEmpty (Set.intersect aCells bCells) then
                    []

                else
                    [ aId, bId ]
            )
        |> Set.fromList


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


{-| Change the zoom level while keeping the viewport centre fixed in
world coordinates (so zooming in/out doesn't shift what you're looking at).
-}
zoomTo : Int -> Model -> Model
zoomTo newU model =
    let
        vpW =
            toFloat (model.windowW * 4 // 5)

        vpH =
            toFloat model.windowH

        oldViewCols =
            vpW / toFloat model.u

        newViewCols =
            vpW / toFloat newU

        oldViewRows =
            vpH / toFloat model.u

        newViewRows =
            vpH / toFloat newU
    in
    { model
        | u = newU
        , panX = model.panX + (oldViewCols - newViewCols) / 2
        , panY = model.panY + (oldViewRows - newViewRows) / 2
    }


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


{-| Compute the actual bounding box of a rule from its children's positions
and sizes, i.e. the smallest axis-aligned box containing all children.
-}
computeRuleBox : SubRule -> ( Int, Int )
computeRuleBox rule =
    let
        extents =
            rule.children
                |> List.filterMap
                    (\c ->
                        case lookupSpec c.kind of
                            Just spec ->
                                let
                                    ( nH, nW ) =
                                        specDims spec

                                    ( ew, eh ) =
                                        effectiveDims nW nH c.rotation
                                in
                                Just ( c.col + ew, c.row + eh )

                            Nothing ->
                                Nothing
                    )

        maxW =
            extents |> List.map Tuple.first |> List.maximum |> Maybe.withDefault 0

        maxH =
            extents |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0
    in
    ( maxW, maxH )


{-| Replace the board with the given rule's children as one cluster,
preserving everything else. If the rule doesn't exist, clear the board.
-}
doShowRule : String -> Model -> Model
doShowRule name model =
    case Dict.get name model.rules of
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


{-| Parse a rule name like "T^4" into ("T", 4). A bare "T" is interpreted
as level 2 (the basic rule). Returns Nothing if the name isn't in a
recognised shape.
-}
parseRuleName : String -> Maybe ( String, Int )
parseRuleName name =
    case String.split "^" name of
        [ kind ] ->
            if String.isEmpty kind then
                Nothing

            else
                Just ( kind, 2 )

        [ kind, nStr ] ->
            case String.toInt nStr of
                Just n ->
                    if n >= 2 && not (String.isEmpty kind) then
                        Just ( kind, n )

                    else
                        Nothing

                Nothing ->
                    Nothing

        _ ->
            Nothing


{-| Per-rule position scaling factor, inferred from the rule's bounding
box divided by the parent kind's native dimensions. For the basic T rule
(12×12 box, T native 6×6) this is 2; for T^3 (24×24), it's 4; etc.
-}
ruleFactor : String -> SubRule -> Int
ruleFactor parentKind rule =
    case lookupSpec parentKind of
        Just spec ->
            let
                ( pH, pW ) =
                    specDims spec

                ( rW, rH ) =
                    computeRuleBox rule

                fw =
                    if pW > 0 then
                        rW // pW

                    else
                        2

                fh =
                    if pH > 0 then
                        rH // pH

                    else
                        2
            in
            max 1 (max fw fh)

        Nothing ->
            2


{-| Inflation substitution using a specific rule suffix (e.g. "" for basic,
"^2" for level-2 etc.). Looks up "<kind>++suffix" in the rules dict. The
factor is inferred from the rule's bounding box, so applying a bigger
rule automatically spaces children further apart.
-}
expandTile : Int -> Dict String SubRule -> String -> PlacedTile -> List PlacedTile
expandTile clusterId rules suffix t =
    let
        ruleKey =
            t.kind ++ suffix
    in
    case Dict.get ruleKey rules of
        Just rule ->
            let
                factor =
                    ruleFactor t.kind rule

                kf =
                    toFloat factor

                ( ruleW, ruleH ) =
                    computeRuleBox rule
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
            -- No rule matches; keep the tile in place, rebadged with the
            -- new clusterId so it still behaves as a draggable unit.
            [ { t | clusterId = clusterId } ]


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


{-| Expand placed tiles into clusters in spatial order.

Place the first cluster at the world origin. For each subsequent cluster:
find the closest already-placed source tile (by source-space distance),
then place the new cluster's bounding box immediately adjacent to that
prior cluster on the side dictated by the source-space direction (right
if this source was to the right, down if below, etc.). Then iteratively
push the cluster along the source-directed axis until it clears any
other prior cluster. This keeps the substituted components packed close
together while preserving the left/right/top/bottom ordering of the
original pattern.

-}
expandInOrderWithFit : Int -> Dict String SubRule -> String -> List PlacedTile -> ( List PlacedTile, Int )
expandInOrderWithFit startCid rules suffix placed =
    let
        ordered =
            placed |> List.sortBy (\t -> ( t.row, t.col ))

        step : PlacedTile -> ( List ( PlacedTile, List PlacedTile ), Int ) -> ( List ( PlacedTile, List PlacedTile ), Int )
        step source ( acc, cid ) =
            let
                initial =
                    expandTile cid rules suffix source

                positioned =
                    if List.isEmpty acc then
                        shiftClusterToOrigin initial

                    else
                        placeNextToClosest source initial acc

                fitted =
                    fitClusterAgainst source positioned acc
            in
            ( acc ++ [ ( source, fitted ) ], cid + 1 )

        ( placedWithSource, finalCid ) =
            List.foldl step ( [], startCid ) ordered

        tiles =
            List.concatMap Tuple.second placedWithSource
    in
    ( tiles, finalCid )


{-| Shift a cluster so its bounding-box top-left is at world (0, 0).
-}
shiftClusterToOrigin : List PlacedTile -> List PlacedTile
shiftClusterToOrigin cluster =
    case tilesBoundingBox cluster of
        Just bbox ->
            cluster
                |> List.map (\t -> { t | col = t.col - bbox.x1, row = t.row - bbox.y1 })

        Nothing ->
            cluster


{-| Given the current source tile and a freshly-expanded cluster, find
the closest already-placed source tile and move the new cluster so its
bounding box sits adjacent to that closest cluster's bbox on the side
dictated by their source-space direction.
-}
placeNextToClosest : PlacedTile -> List PlacedTile -> List ( PlacedTile, List PlacedTile ) -> List PlacedTile
placeNextToClosest source cluster prior =
    let
        closest =
            prior
                |> List.sortBy
                    (\( src, _ ) ->
                        let
                            dx =
                                src.col - source.col

                            dy =
                                src.row - source.row
                        in
                        dx * dx + dy * dy
                    )
                |> List.head
    in
    case closest of
        Just ( closestSrc, closestCluster ) ->
            placeAdjacentTo closestSrc source closestCluster cluster

        Nothing ->
            cluster


{-| Translate `curCluster` so that its bounding box sits immediately on
the correct side of `prevCluster`'s bounding box, with a small buffer.
Side is chosen from the source-position vector (prevSrc -> curSrc).
-}
placeAdjacentTo : PlacedTile -> PlacedTile -> List PlacedTile -> List PlacedTile -> List PlacedTile
placeAdjacentTo prevSrc curSrc prevCluster curCluster =
    case ( tilesBoundingBox prevCluster, tilesBoundingBox curCluster ) of
        ( Just prevBbox, Just curBbox ) ->
            let
                dx =
                    curSrc.col - prevSrc.col

                dy =
                    curSrc.row - prevSrc.row

                buffer =
                    2.0

                curW =
                    curBbox.x2 - curBbox.x1

                curH =
                    curBbox.y2 - curBbox.y1

                ( targetX, targetY ) =
                    if abs dx >= abs dy then
                        if dx >= 0 then
                            ( prevBbox.x2 + buffer, prevBbox.y1 )

                        else
                            ( prevBbox.x1 - buffer - curW, prevBbox.y1 )

                    else if dy >= 0 then
                        ( prevBbox.x1, prevBbox.y2 + buffer )

                    else
                        ( prevBbox.x1, prevBbox.y1 - buffer - curH )

                shiftX =
                    targetX - curBbox.x1

                shiftY =
                    targetY - curBbox.y1
            in
            curCluster
                |> List.map (\t -> { t | col = t.col + shiftX, row = t.row + shiftY })

        _ ->
            curCluster


{-| Iteratively shift `cluster` until it doesn't overlap any previously
placed cluster. Push direction is chosen from the source-tile vector.
-}
fitClusterAgainst : PlacedTile -> List PlacedTile -> List ( PlacedTile, List PlacedTile ) -> List PlacedTile
fitClusterAgainst source cluster prior =
    let
        maxIter =
            200

        go n c =
            if n <= 0 then
                c

            else
                case findOverlappingPrior c prior of
                    Nothing ->
                        c

                    Just ( prevSource, prevCluster ) ->
                        case ( tilesBoundingBox prevCluster, tilesBoundingBox c ) of
                            ( Just prevBbox, Just curBbox ) ->
                                let
                                    pushVec =
                                        sourceDirectedPush prevSource source prevBbox curBbox
                                in
                                go (n - 1) (shiftCluster pushVec c)

                            _ ->
                                c
    in
    go maxIter cluster


findOverlappingPrior : List PlacedTile -> List ( PlacedTile, List PlacedTile ) -> Maybe ( PlacedTile, List PlacedTile )
findOverlappingPrior cluster prior =
    case tilesBoundingBox cluster of
        Nothing ->
            Nothing

        Just curBbox ->
            prior
                |> List.filter
                    (\( _, pc ) ->
                        case tilesBoundingBox pc of
                            Just pBbox ->
                                bboxesOverlap curBbox pBbox

                            Nothing ->
                                False
                    )
                |> List.head


{-| Choose a push vector by comparing source-tile positions. If the current
source is to the right of (or same as) the overlapped prior source, push
right; if below, push down; etc. Magnitude clears the overlap plus a small
buffer.
-}
sourceDirectedPush : PlacedTile -> PlacedTile -> BBox -> BBox -> ( Float, Float )
sourceDirectedPush prevSrc curSrc prevBbox curBbox =
    let
        buffer =
            2.0

        dx =
            curSrc.col - prevSrc.col

        dy =
            curSrc.row - prevSrc.row
    in
    if abs dx >= abs dy then
        if dx >= 0 then
            ( prevBbox.x2 - curBbox.x1 + buffer, 0 )

        else
            ( -(curBbox.x2 - prevBbox.x1 + buffer), 0 )

    else if dy >= 0 then
        ( 0, prevBbox.y2 - curBbox.y1 + buffer )

    else
        ( 0, -(curBbox.y2 - prevBbox.y1 + buffer) )


{-| Repeatedly find the first overlapping pair and push the later one along
its minimum translation axis, until no cluster pair overlaps or an iteration
cap is hit. Kept for fallback; the source-ordered fitter above is preferred.
-}
resolveClusterOverlaps : List (List PlacedTile) -> List (List PlacedTile)
resolveClusterOverlaps initial =
    let
        maxIterations =
            20000

        loop : Int -> List (List PlacedTile) -> List (List PlacedTile)
        loop n clusters =
            if n <= 0 then
                clusters

            else
                case firstOverlappingPair clusters of
                    Nothing ->
                        clusters

                    Just ( _, jLater, push ) ->
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
scale / factor, using the rule found under "<kind>++suffix".
-}
deflateTile : Int -> Dict String SubRule -> String -> PlacedTile -> List PlacedTile
deflateTile clusterId rules suffix t =
    let
        ruleKey =
            t.kind ++ suffix
    in
    case Dict.get ruleKey rules of
        Just rule ->
            let
                factor =
                    ruleFactor t.kind rule

                childScale =
                    t.scale / toFloat factor

                ( ruleW, ruleH ) =
                    computeRuleBox rule
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
    , captureName : String
    , applySuffix : String
    , overlappingClusters : Set Int
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
      , captureName = ""
      , applySuffix = ""
      , overlappingClusters = Set.empty
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
    | UpdateCaptureName String
    | UpdateApplySuffix String
    | BuildRule String



-- ============================ Update ============================


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( intermediate, baseCmd ) =
            baseUpdate msg model

        -- Recompute per-cluster overlap status on any message except live
        -- drag updates. That means the red highlight stays on while you
        -- drag an already-overlapping cluster, and is re-evaluated the
        -- moment you release the mouse.
        newModel =
            case msg of
                DragMouseMove _ _ ->
                    intermediate

                _ ->
                    { intermediate
                        | overlappingClusters =
                            computeOverlappingClusters intermediate.placed
                    }

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
            zoomTo (min maxU (model.u + zoomStep model.u)) model

        ZoomOut ->
            zoomTo (max minU (model.u - zoomStep model.u)) model

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

                        maxCid =
                            newTiles
                                |> List.map .clusterId
                                |> List.maximum
                                |> Maybe.withDefault -1

                        mergedRules =
                            Dict.union saved.rules model.rules
                    in
                    recenterOnTiles
                        { model
                            | placed = newTiles
                            , nextId = startId + List.length saved.tiles
                            , nextClusterId = max model.nextClusterId (maxCid + 1)
                            , selectedPlaced = Nothing
                            , selectedKind = Nothing
                            , drag = Nothing
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

        ShowRule name ->
            doShowRule name model

        ApplyAll ->
            let
                ( newTiles, nextCid ) =
                    expandInOrderWithFit
                        model.nextClusterId
                        model.rules
                        model.applySuffix
                        model.placed

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

        UpdateCaptureName n ->
            { model | captureName = n }

        UpdateApplySuffix s ->
            { model | applySuffix = s }

        BuildRule name ->
            case parseRuleName name of
                Just ( kind, level ) ->
                    -- Step 1: if we already have this rule captured, just show it.
                    case Dict.get name model.rules of
                        Just _ ->
                            doShowRule name model

                        Nothing ->
                            if level <= 2 then
                                -- "T" or "T^2": basic rule, just show.
                                doShowRule kind model

                            else
                                -- "T^N" with N > 2: load the basic rule's
                                -- children, then substitute each tile by
                                -- its kind^(N-1) rule, using the
                                -- source-ordered fitter so clusters stay
                                -- in roughly the spatial order of the
                                -- parent pattern.
                                let
                                    shown =
                                        doShowRule kind model

                                    suffix =
                                        "^" ++ String.fromInt (level - 1)

                                    ( newTiles, nextCid ) =
                                        expandInOrderWithFit
                                            shown.nextClusterId
                                            shown.rules
                                            suffix
                                            shown.placed

                                    ( withIds, count ) =
                                        renumber newTiles
                                in
                                recenterOnTiles
                                    { shown
                                        | placed = withIds
                                        , nextId = count
                                        , nextClusterId = nextCid
                                        , selectedPlaced = Nothing
                                        , selectedKind = Nothing
                                    }

                Nothing ->
                    model

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
                                    deflateTile cid model.rules model.applySuffix tile

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
            let
                isOverlapping =
                    Set.member p.clusterId model.overlappingClusters
            in
            drawTile isOverlapping model.u True (model.selectedPlaced == Just p.id) p spec

        Nothing ->
            []


drawTile : Bool -> Int -> Bool -> Bool -> PlacedTile -> TileSpec -> List (Svg Msg)
drawTile isOverlapping u_ onBoard isSelected p spec =
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

        decorationList =
            spec.decorations
                |> List.map
                    (\d ->
                        let
                            ( rotC, rotR ) =
                                rotateCell p.rotation (specDims spec) ( d.col, d.row )

                            ( px, py ) =
                                cellPx ( rotC, rotR )
                        in
                        rect
                            [ SA.x (String.fromFloat px)
                            , SA.y (String.fromFloat py)
                            , SA.width (String.fromFloat cellSz)
                            , SA.height (String.fromFloat cellSz)
                            , SA.fill (decorationColorFor d.kind)
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

        overlapHighlight =
            if isOverlapping && onBoard then
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
                                , SA.stroke "#dc143c"
                                , SA.strokeWidth "2.5"
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
    clipDef :: List.map cellRect localCells ++ bandList ++ markerList ++ decorationList ++ selection ++ overlapHighlight ++ [ letter ]



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
            ]
        , div [ HA.class "controls" ]
            [ button [ HE.onClick (ShowRule "A") ] [ text "Show A" ]
            , button [ HE.onClick (ShowRule "R") ] [ text "Show R" ]
            , button [ HE.onClick (ShowRule "T") ] [ text "Show T" ]
            ]
        , div [ HA.class "controls" ]
            [ input
                [ HA.type_ "text"
                , HA.placeholder "rule name (e.g. T^3)"
                , HA.value model.captureName
                , HE.onInput UpdateCaptureName
                ]
                []
            , button [ HE.onClick (CaptureRule model.captureName) ] [ text "Capture as" ]
            , button [ HE.onClick (ShowRule model.captureName) ] [ text "Show" ]
            , button [ HE.onClick (BuildRule model.captureName) ] [ text "Build" ]
            ]
        , div [ HA.class "controls" ]
            [ input
                [ HA.type_ "text"
                , HA.placeholder "apply suffix (e.g. ^2)"
                , HA.value model.applySuffix
                , HE.onInput UpdateApplySuffix
                , HA.style "width" "90px"
                ]
                []
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
        entries =
            Dict.toList rules
                |> List.sortBy Tuple.first

        row ( name, rule ) =
            p [ HA.class "status" ]
                [ text (name ++ ": " ++ String.fromInt (List.length rule.children) ++ " children") ]
    in
    if List.isEmpty entries then
        p [ HA.class "status" ] [ text "no rules captured" ]

    else
        div [] (List.map row entries)


paletteEntry : Model -> TileSpec -> Html Msg
paletteEntry model spec =
    let
        isSel =
            model.selectedKind == Just spec.name

        ( h, w ) =
            specDims spec

        pu =
            7
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
            (drawTile False
                pu
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
