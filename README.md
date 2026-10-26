# wordperfect-graphics

A Common Lisp reader for WordPerfect Graphics (`.wpg`) files, versions 1 (WordPerfect 5.x) and 2 (WordPerfect 6 and later), plus a `wpg` command-line tool for inspecting files, extracting bitmaps, and converting to SVG.

The library itself has no dependencies.

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
(asdf:load-system :wordperfect-graphics)
```

### Systems

| System | Contents | Depends on |
|---|---|---|
| `wordperfect-graphics` | parser, bitmap decoding, SVG export, PPM output | nothing |
| `wordperfect-graphics/png` | `write-bitmap-png`, `bitmap-png-octets` | zpng |
| `wordperfect-graphics/tests` | FiveAM suite | fiveam |
| `wordperfect-graphics/cli` | the `wpg` program | clingon |

## Command line

```
wpg info FILE...                                   summarize files
wpg records [-v] [-x] FILE                         list records (-v: full lists, -x: hex dump)
wpg extract [-o DIR] [-f png|ppm] [-p] [--raw] FILE
                                                   write bitmaps (-p: also embedded PostScript;
                                                   --raw: without the file's mirroring/rotation)
wpg svg [-o OUT|-] [--no-bitmaps] FILE             convert to SVG, bitmaps embedded as PNG
```

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

Exit status is 0 on success, 1 for an unreadable file or bad arguments, and 64 for an unknown command.

## Library

```lisp
(let ((image (wpg:read-wpg "chart.wpg"))      ; pathname, filename, stream or octet vector
  (wpg:wpg-image-version image)                ; 1 or 2
  (wpg:wpg-image-width-inches image)
  (loop for r across (wpg:wpg-image-records image)
        do (format t "~A ~S~%" (wpg:wpg-record-name r) (wpg:wpg-record-fields r)))
  (dolist (b (wpg:wpg-image-bitmaps image))
    (wpg:bitmap-pixel b 0 0)                   ; => a WPG:COLOR
    (wpg:bitmap-rgba-octets b)                 ; flat RGBA, top row first
    (wpg:bitmap-rgba-octets b :oriented t)     ; ... mirrored/rotated as the file says
    (wpg:write-bitmap-ppm b #p"out.ppm"))
  (with-open-file (s #p"chart.svg" :direction :output :if-exists :supersede)
    (wpg:write-svg image s)))
```

- **Filenames.** A string filename is taken as a native filename, so `[` and `*` in it are ordinary characters.
- **Records.** Every record is kept with its offset, type, length and raw `body`. Known types also get a keyword `name` and a `fields` plist, which `record-field` reads. For example, a WPG1 `:line` has `:x1 :y1 :x2 :y2`, and a WPG2 `:polyline` has `:characterization` (flags plus an affine `:matrix`) and `:points`.
- **Nesting.** WPG2 records nest: a record's `extension` is the number of child records that follow it. `wpg-record-parent` and `wpg-record-depth` give the structure, and `record-children` lists a record's children. A compound polygon's children are the outlines that make up its one shape.
- **Bitmaps** are decoded as they are read, using the palette in effect at that point in the file. `wpg-bitmap-format` is `:indexed` (1, 2, 4 or 8 bits), `:rgb`, `:rgba`, `:gray`, `:gray16` or `:rgb555`. For WPG1, `wpg-bitmap-x1`…`y2` give the placement from Bitmap (Type 2) records; for WPG2 they come from the Bitmap object record.
- **Damage.** By default, damage is tolerated. A record that fails to decode keeps its raw body and gets an `error`, and problems are listed in `wpg-image-warnings`. Pass `:strict t` to signal the first problem instead. Errors are subclasses of `wpg-error`: `wpg-format-error` (not a WPG file, encrypted, or an unknown version), `wpg-truncated-error`, and `wpg-decode-error`. `*max-bitmap-bytes*` (256 MB) caps what a damaged header can make the reader allocate.
- **Text.** WPG text is stored as WordPerfect document streams. `decode-wp5-text` (WPG1 Graphics Text type 2) returns a string, and `decode-wp6-text` (WPG2 Text Data, in the record's `:runs` field) returns runs with font size in points, color and typeface. Characters in WordPerfect's multinational, typographic, box-drawing and iconic sets are mapped to Unicode; others become `?`.
- **SVG.** `write-svg` takes `:bitmap-href`, a function from a bitmap to a URI (or `NIL` to omit it). The CLI passes one that makes PNG `data:` URIs.

## Format coverage

**WPG1:**
- **Decoded:** fill, line and marker attributes; polymarker, line, polyline, rectangle, polygon, ellipse/arc and curved polyline (Bézier); bitmap types 1 and 2; graphics text types 1 and 2 and text attributes; color map; start/end; and the bounding boxes and payload of PostScript types 1 and 2.
- **Named but kept raw:** output attributes, figure/chart/PlanPerfect data, and graphics text type 3.

**WPG2:**
- **Decoded:** start/end, color palette (8- and 16-bit), bitmap object and bitmap data, polyline, polyspline, polycurve, rectangle, arc, compound polygon, pen and brush colors (including gradient colors), brush gradient, pen size, pen style and pen style definitions, line cap/join, brush pattern, text line, text block and text data.
- **Named but kept raw:** about 25 other types, including text paths, groups and layers. Groups still nest correctly.

**Compression:** both run-length schemes, including WPG2 sample sizes, row repeats and XOR rows.

**SVG export** covers the shapes, colors, pen widths and bitmaps listed above. It also handles:
- WPG2 compound polygons (drawn as one even-odd path) and groups;
- two-color brush gradients;
- dashed lines: WPG2 pen styles, built-in or defined in the file, and WPG1 line styles;
- text, with its size, color and rotation, and for WPG1 the stored character width.

It does not render:
- **Fill patterns**, which are drawn solid: WPG1 fill styles other than 0 and 1, and WPG2 brush patterns. Nothing available shows what they should look like, and LibreOffice draws them solid too.
- **Gradients with more than two colors**, drawn in their first color. None turned up in the files tested.
- **Text paths and object images.**

Typefaces are approximated by generic families (sans-serif, serif, monospace).

## Notes on the format

There is no public specification. This follows [libwpg](https://sourceforge.net/projects/libwpg/), which LibreOffice and Inkscape use, and ImageMagick's `coders/wpg.c`. Where they differ:

- **WPG1 default palette.** This is the VGA palette. Entries 16–255 match libwpg. Entries 0–15 are the EGA colors WordPerfect for DOS displayed, as in ImageMagick; libwpg uses a Windows-style set there.
- **WPG1 monochrome bitmaps** use the file's palette if it has one, otherwise black and white. A palette whose first two entries are both black is repaired, as ImageMagick does. libwpg always uses black and white.
- **WPG2 bitmaps without a palette** use libwpg's default WPG2 palette. ImageMagick uses grayscale.
- **PostScript in WPG1 type 2 records** is located by its `%!` or EPS signature, because libwpg and ImageMagick disagree on the header length.
- **WPG2 transparency.** Palette and pen/brush colors store transparency; `color-alpha` is opacity (255 = opaque). 32-bit RGBA bitmaps are read as opacity, as libwpg does.

### Where SVG output differs from libwpg

Checked against LibreOffice (which uses libwpg) on the 4,596 clip-art files from the Corel WordPerfect 7 for Unix CD:

- **Multi-part compound polygons are filled.** LibreOffice leaves a compound polygon unfilled when it has more than one child outline, though the file marks every part filled and sets the brush color just before it. An example is a pair of shoes stored as one compound.
- **A solid brush color ends a gradient.** In libwpg, once a gradient is set it stays on, washing out every later fill in a drawing.
- **Brush Gradient records are 9 bytes**, ending in a 1-byte flags field. libwpg reads 12.
- **WPG1 text uses its attributes record.** Text gets its stored size, color, alignment and angle; libwpg ignores the attributes. Graphics Text type 2 records are drawn only when the file has no type 1 copy of the text, which DrawPerfect always writes.

Also checked against LibreOffice: 1,019 WPG1 files from DrawPerfect and WordPerfect 5.1 clip-art disks on the Internet Archive.

### Guesses

These follow the most plausible reading but are unconfirmed:
- **Dash lengths** follow libwpg: a pen style value V is about 1.19 × V pen widths long. This matches LibreOffice exactly, but WordPerfect may have drawn them differently.
- **WPG1 line styles** 2 and up are taken to be WPG2 pen styles 1 and up.
- **Vertical text alignment** codes 1–3 are taken as top, middle and bottom.
- **Gradient direction** is unverified.

## Tests

`make test` runs about 300 checks. Most use WPG files built byte by byte in `tests/builder.lisp`. Two real files from other projects' test suites live in `tests/data/`; see `tests/data/NOTICE` for their sources and licenses.
