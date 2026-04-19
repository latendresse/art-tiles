# ART Tiles

A small Elm visualizer for laying out three jigsaw-style tiles — **A**, **R**, **T** — on a grid and testing tilings by hand.

## What it does

- **Palette on the right**: the three tile kinds. Click one to select.
- **Board on the left**: a pixel grid. With a tile selected, click anywhere on the board to place a copy.
- **Select a placed tile**: click it (orange outline appears).
- **Move**: click-and-drag a placed tile.
- **Rotate**: button rotates the selected tile 90° clockwise (or, if nothing placed is selected, changes the orientation for the next placement).
- **Delete / Clear**: remove the selected tile, or wipe the board.

## Tile data

Each tile is specified by an ASCII grid (`#` = filled pixel, `.` = cut) plus a row range for the horizontal yellow band that carries the path and the letter. See `src/Main.elm` — the three `TileSpec` records near the top are the full spec.

| Tile | Size | Color |
|------|------|-------|
| A    | 8×8  | light blue |
| R    | 8×8  | red / pink |
| T    | 6×6  | light green |

## Running locally

```sh
# build the JS from Elm
elm make src/Main.elm --output=elm.js

# serve static files
python3 -m http.server 8050
```

Then open <http://localhost:8050>.

## Layout

```
art-tiles/
├── elm.json          -- Elm project config
├── index.html        -- HTML entry point, mounts the Elm app
├── style.css         -- layout & control styling (Elm handles the SVG)
├── elm.js            -- compiled Elm output
├── src/
│   └── Main.elm      -- the whole app
└── README.md
```
