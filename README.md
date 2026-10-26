# wordperfect-graphics

A Common Lisp reader for WordPerfect Graphics (`.wpg`) files, versions 1 (WordPerfect 5.x) and 2 (WordPerfect 6 and later), with SVG and PNG output, plus a `wpg` command-line tool for inspecting files, extracting bitmaps, and converting whole directories.

The core library has no dependencies.

## Building

Dependencies are vendored with [ocicl](https://github.com/ocicl/ocicl):

```sh
ocicl install          # fetch what ocicl.csv lists into ocicl/
make                   # build bin/wpg
make test              # run the FiveAM suite
make clean             # remove bin/ and this tree's fasl cache
```

The Makefile uses a hermetic source registry (this tree only). From your own REPL, with ocicl set up in `~/.sbclrc`:

```lisp
(asdf:load-system :wordperfect-graphics)          ; or .../raster for PNG rendering
```

### Systems

| System | Contents | Depends on |
|---|---|---|
| `wordperfect-graphics` | parser, bitmap decoding, drawing model, SVG export, PPM output | nothing |
| `wordperfect-graphics/png` | `write-bitmap-png`, `bitmap-png-octets` | zpng |
| `wordperfect-graphics/raster` | `render-scene`, `render-png`: whole drawings to pixels | cl-vectors, zpb-ttf |
| `wordperfect-graphics/tests` | FiveAM suite | fiveam |
| `wordperfect-graphics/cli` | the `wpg` program | clingon |

## Command line

```
wpg info [--json] [-r] FILE|DIR...                 summarize files
wpg records [-v] [-x] [--json] FILE                list records (-v: full lists, -x: hex dump)
wpg svg [-o OUT] [--no-bitmaps] [-r] FILE|DIR...   convert to SVG, bitmaps embedded as PNG
wpg png [-o OUT] [-w PX] [--height PX] [--dpi N] [--transparent] [-r] FILE|DIR...
                                                   render to PNG (96 dpi unless a size is given)
wpg extract [-o DIR] [-f png|ppm] [-p] [--raw] [-r] FILE|DIR...
                                                   write bitmaps (-p: also embedded PostScript;
                                                   --raw: without the file's mirroring/rotation)
```

- **Batch use.** Every command but `records` takes several files and directories. A directory means its `*.wpg` files; with `-r`, its subdirectories' files too.
- **Output.** By default, output goes next to each input. With `-o DIR`, files found in a directory keep their relative paths under `DIR`. `-o FILE` works for a single input, and `wpg svg -o -` writes to standard output.
- **Failures.** A file that fails is reported and the rest carry on.
- **Exit status.** 0 on success; 1 if any file failed or the arguments are wrong; 64 for an unknown command.
- **JSON.** `info --json` writes one JSON object per file, one per line. `records --json` writes one document with every record and its decoded fields.

```
$ wpg info tests/data/imagemagick-input.wpg
tests/data/imagemagick-input.wpg: WPG version 2.0
  size:     6728 x 7762 units (5.61 x 6.47 in at 1200/in)
  records:  18 (0 of unknown type, 0 failed to decode)
  kinds:    bitmap-data×2, bitmap×2, brush-back-color, ...
  bitmaps:  2
    #0  32x32, 1-bit, rle, 114x114 dpi, at (7,0)-(506,549)
    #1  32x32, 24-bit, none, 114x114 dpi, at (7,0)-(506,549)
```

## Library

```lisp
(let ((image (wpg:read-wpg "chart.wpg")))      ; pathname, filename, stream or octet vector
  (wpg:wpg-image-version image)                ; 1 or 2
  (loop for r across (wpg:wpg-image-records image)
        do (format t "~A ~S~%" (wpg:wpg-record-name r) (wpg:wpg-record-fields r)))
  (dolist (b (wpg:wpg-image-bitmaps image))
    (wpg:bitmap-rgba-octets b :oriented t))    ; flat RGBA, mirrored/rotated as the file says
  (with-open-file (s #p"chart.svg" :direction :output :if-exists :supersede)
    (wpg:write-svg image s))
  (wpg:render-png image #p"chart.png" :width 800))   ; needs wordperfect-graphics/raster
```

- **Reading.** `read-wpg` accepts a pathname, filename, binary stream or octet vector.
  - A string filename is taken as a native filename, so `[` and `*` in it are ordinary characters.
  - Corel's suites sometimes saved a WPG inside an OLE2 compound document (the container format of old Office files). Those are read from the container's `PerfectOffice_MAIN` stream, and `wpg-image-container` returns `:ole`.
- **Records.** Every record is kept with its offset, type, length and raw `body`. Known types also get a keyword `name` and a `fields` plist, which `record-field` reads.
  - Examples: a WPG1 `:line` has `:x1 :y1 :x2 :y2`; a WPG2 `:polyline` has `:characterization` (flags plus an affine `:matrix`) and `:points`.
  - WPG2 records nest: a record's `extension` is the number of child records that follow it. `wpg-record-parent` and `wpg-record-depth` give the structure, and `record-children` lists a record's children.
- **Bitmaps** are decoded as they are read, using the palette in effect at that point in the file. `wpg-bitmap-format` is `:indexed` (1, 2, 4 or 8 bits), `:rgb`, `:rgba`, `:gray`, `:gray16` or `:rgb555`.
- **Text** is stored as WordPerfect document streams. `decode-wp5-text` (WPG1) returns a string. `decode-wp6-text` (WPG2, in a Text Data record's `:runs`) returns runs with font size, color and typeface. Characters in WordPerfect's multinational, typographic, box-drawing and iconic sets are mapped to Unicode; others become `?`.
- **The drawing model.** `build-scene` replays the records into a `scene`: the page and a list of shapes.
  - Each shape carries a transform and a resolved `style`: a fill that is a color, `gradient` or `texture`; a fill rule; a stroke; a width; dashes.
  - The shape kinds are line, poly, rect, ellipse, path, text and image.
  - The SVG writer and the rasterizer both draw a scene, so another output format only has to draw these shapes.
- **SVG.** `write-svg` takes `:bitmap-href`, a function from a bitmap to a URI (or `NIL` to leave bitmaps out). The CLI passes one that makes PNG `data:` URIs.
- **PNG.** `render-png` and `render-scene` draw with anti-aliasing.
  - Text uses system TrueType fonts (Arial/Helvetica/DejaVu Sans, and serif and monospace equivalents); see `*font-files*`. With no font found, text is skipped.
  - Lines are never drawn thinner than a pixel.
- **Damage.** By default, damage is tolerated.
  - A record that fails to decode keeps its raw body and gets an `error`, and problems are listed in `wpg-image-warnings`. Pass `:strict t` to signal the first problem instead.
  - Errors are subclasses of `wpg-error`: `wpg-format-error`, `wpg-truncated-error` and `wpg-decode-error`.
  - `*max-bitmap-bytes*` and `*max-pixels*` cap what a damaged file can make the reader or renderer allocate.

## Format coverage

**WPG1:**
- **Decoded:**
  - shapes: polymarker, line, polyline, rectangle, polygon, ellipse/arc, curved polyline (Bézier), and the closed curved polygon WordPerfect 8 writes (type `0x1F`);
  - attributes: fill, line and marker attributes;
  - bitmaps: types 1 and 2;
  - text: graphics text types 1 and 2, and text attributes;
  - other: color map, start/end, and the bounding boxes and payload of PostScript types 1 and 2.
- **Named but kept raw:** output attributes, figure/chart/PlanPerfect data, and graphics text type 3. WordPerfect 8's other undocumented types (`0x23`, `0x24`, `0x2E`…`0x33`) are kept raw too.

**WPG2:**
- **Decoded:**
  - structure: start/end, groups (with their transforms), compound polygon;
  - shapes: polyline, polyspline, polycurve, rectangle, arc;
  - bitmaps: bitmap object and bitmap data;
  - color: color palette (8- and 16-bit), pen and brush colors (including gradient colors), brush gradient, texture definitions and brush textures;
  - pens: pen size, pen style and pen style definitions, line cap/join, brush pattern;
  - text: text line, text block and text data.
- **Named but kept raw:** about 20 other types, including text paths, layers and object capsules.

**Compression:** both run-length schemes, including WPG2 sample sizes, row repeats and XOR rows.

**Output (SVG and PNG)** covers everything decoded above, including:
- compound polygons, drawn as one even-odd path;
- nested group transforms;
- linear, radial and square gradients;
- tiled textures;
- dashed lines;
- text with its size, color and rotation;
- page backgrounds: a gradient or texture brush in the Start WPG page attributes fills the page, as in WordPerfect 8's background files.

**Not drawn:**
- fill patterns, which are drawn solid;
- text paths and object images. No text path has turned up in any of the files tested.

Typefaces are approximated by generic families.

## Notes on the format

There is no public specification. This follows [libwpg](https://sourceforge.net/projects/libwpg/), which LibreOffice and Inkscape use, and ImageMagick's `coders/wpg.c`. Where they differ:

- **WPG1 default palette.** This is the VGA palette. Entries 16–255 match libwpg. Entries 0–15 are the EGA colors WordPerfect for DOS displayed, as in ImageMagick; libwpg uses a Windows-style set there.
- **WPG1 monochrome bitmaps** use the file's palette if it has one, otherwise black and white. A palette whose first two entries are both black is repaired, as ImageMagick does.
- **WPG2 bitmaps without a palette** use libwpg's default WPG2 palette.
- **PostScript in WPG1 type 2 records** is located by its `%!` or EPS signature, because libwpg and ImageMagick disagree on the header length.
- **WPG2 transparency.** Palette and pen/brush colors store transparency; `color-alpha` is opacity (255 = opaque).

### Where output differs from libwpg

Checked against LibreOffice (which uses libwpg) on about 18,000 files:
- 4,596 clip-art files from the Corel WordPerfect 7 for Unix CD;
- 1,019 WPG1 files from DrawPerfect and WordPerfect 5.1 clip-art disks;
- 1,325 files from the WordPerfect Suite 8 and Office 2000 CDs;
- 10,041 clip-art files from the WordPerfect Office 11 CDs.

All but the Corel 7 files are on the Internet Archive.

- **Groups are drawn through their transforms.** libwpg ignores group records, so drawings built from scaled groups (an area-code map of the US, for one) come out nearly empty in LibreOffice.
- **Multi-part compound polygons are filled.** LibreOffice leaves a compound polygon unfilled when it has more than one child outline, though every part is marked filled.
- **A solid brush color ends a gradient.** In libwpg, once a gradient is set it stays on, washing out later fills.
- **Page attributes that set a gradient or texture fill the page.** libwpg ignores them, so WordPerfect 8's background files come out blank.
- **Textures** are tiled; libwpg does not draw them.
- **WPG1 text uses its attributes record:** stored size, color, alignment and angle. Graphics Text type 2 records are drawn only when there is no type 1 copy of the text, which DrawPerfect always writes.
- **WordPerfect 8's WPG1 type `0x1F`** is drawn. Every one of the 180 seen is a closed cubic Bézier path (3n+1 points, last equal to first); WordPerfect 8's border files are drawn with them.

Corrections to the layout libwpg reads:
- **Brush Gradient records are 9 bytes**, ending in a 1-byte flags field; libwpg reads 12.
- **Start WPG's image bounds** are sometimes leftover bytes (unordered, or zero-width) in files WordPerfect 8 wrote. The viewport is used as the page then.

### Guesses

These follow the most plausible reading but are unconfirmed:
- **Dash lengths** are taken to be in file units, whatever the pen width. In WordPerfect Office 11's clip art, two motion trails define the same dash (275 on, 165 off, at 1000 units per inch). Read in file units, both get the quarter-inch dashes the artist evidently meant. libwpg scales by the pen width instead (about 1.19 × value × width), which makes them 18 inches long. It also reads the width before the file sets it, so LibreOffice draws one trail dotted and the other solid.
- **WPG1 line styles** 2 and up are taken to be WPG2 pen styles 1 and up.
- **Vertical text alignment** codes 1–3 are taken as top, middle and bottom.
- **Gradient angles** follow libwpg: 0 runs top to bottom, larger angles turn clockwise. This matches LibreOffice on WordPerfect 8's gradient backgrounds.
- **Gradient types.** Circle, ellipse, square and rectangle gradients spread from the brush's reference point (a fraction of the shape's box, from its top-left) to the farthest corner, with the first color at the center. Highlights on WordPerfect 8's gem shapes land at the upper left, which supports this reading. SVG approximates squares and rectangles with radial gradients.
- **Gradients with more than two colors** get evenly spaced stops. None have turned up in any file.
- **Texture tiles** are taken to be 96 pixels per inch.
- **Hairlines.** Pens thinner than 1/300 inch are drawn as hairlines, as WordPerfect would have on screen.

## Tests

`make test` runs about 360 checks. Most use WPG files built byte by byte in `tests/builder.lisp`, including OLE2 containers. Two real files from other projects' test suites live in `tests/data/`; see `tests/data/NOTICE` for their sources and licenses.
