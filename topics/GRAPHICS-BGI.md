# Graphics (BGI)

BGI usage on TP7 (and DOS targets) with notes on modern replacements.
Compatibility: TP5+, TP7 (Graph unit/BGI). Not available in TP3. FPC TP mode only on targets that ship Graph; consider SDL_BGI/WinBGI for modern OSes.

## Initialization Patterns
- Typical flow: `DetectGraph(Driver, Mode); InitGraph(Driver, Mode, 'path');`.
- Keep BGI drivers in the executable directory or set `BGI` environment variable.
- Basic example (adapted for FPC TP mode):

Compatibility: TP5+, TP7 (BGI); FPC TP mode only on targets that ship `Graph`
```pascal
{$IFDEF FPC}{$MODE TP}{$ENDIF}
program SimpleBGI;
uses Crt, Graph;
var
  Driver, Mode: Integer;
begin
  Driver := Detect;
  InitGraph(Driver, Mode, ''); { driver in current directory }
  if GraphResult <> grOk then
    Halt(1);
  SetColor(Yellow);
  Line(0, 0, GetMaxX, GetMaxY);
  ReadKey;
  CloseGraph;
end.
```

## Video Modes
- Common modes (BGI constants):
  - `CGA`: `CGA320x200` (320x200x4), `CGA640x200` (640x200x2)
  - `EGA`: `EGA320x200` (320x200x16), `EGA640x200` (640x200x16), `EGA640x350` (640x350x16)
  - `VGA`: `VGA320x200` (320x200x256), `VGA640x480` (640x480x16)
  - `HERCMONO`: 720x348 mono
  - `ATT400`: 640x400x2
  - `IBM8514`: 640x480x256, 1024x768x256 (requires 8514 driver)
- Drivers shipped with TP7: `CGA`, `EGAVGA`, `HERC`, `ATT`, `PC3270`, `IBM8514`.
- Some original TP examples accept `/H` on the command line to force the highest
  available resolution; keep this pattern to auto-pick the max `GraphMode`.

## Drawing Primitives
- Lines, circles, rectangles, text output; ensure examples check `GraphResult` after init.
- Fonts and lines: TP examples often include arrays of `Fonts`, `LineStyles`,
  `FillStyles` to enumerate values for `SetTextStyle`, `SetLineStyle`,
  `SetFillStyle` (see summary tables below).

## Compatibility Notes
- BGI is TP7/DOS-focused; FPC can use `Graph` unit on DOS targets but consider SDL_BGI/WinBGI for modern OSes.
- Verify memory footprint of page buffers against TP7 64KB limits.
- To remove the dependency on `.BGI`/`.CHR` files, use the official pattern:
  link `.OBJ` generated with `BINOBJ`, call `RegisterBGIdriver`/`RegisterBGIfont`
  before `InitGraph`. Document the build steps and the drivers used (`CGA`,
  `EGAVGA`, `HERC`, `ATT`, `PC3270`).
- Embedded drivers/fonts (TP7 docs):
  1. `BINOBJ CGA.BGI CGA.OBJ` (repeat for each driver) and link the `.OBJ`. Fonts: `BINOBJ TRIP.CHR TRIP.OBJ`. **Symbol naming**: BINOBJ generates symbols like `CGA_driver` from `CGA.BGI` and `TRIP_font` from `TRIP.CHR`.
  2. Call `RegisterBGIdriver(@CGA_driver)` (symbol from BINOBJ) before `InitGraph` (or load `.BGI` into heap and register pointer).
  3. For fonts, link the `.OBJ`, then `RegisterBGIfont(@TRIP_font)` before `InitGraph`/`SetTextStyle`.
  4. Pass a blank path (`''`) to `InitGraph` or point it at `.`; `BGI` env var also works for external drivers.

## Minimal build recipe (no shipped assets)
- Choose the drivers/fonts you need (e.g., `CGA.BGI`, `EGAVGA.BGI`, `TRIP.CHR`). Convert each with `BINOBJ`:
  - `BINOBJ CGA.BGI CGA.OBJ`
  - `BINOBJ EGAVGA.BGI EGAVGA.OBJ`
  - `BINOBJ TRIP.CHR TRIP.OBJ`
- Add the resulting `.OBJ` files to your TP7 project/link step so the symbols
  like `CGA_driver`/`TRIP_font` are available to your program.
- In code (not provided here), call `RegisterBGIdriver(@CGA_driver)` /
  `RegisterBGIfont(@TRIP_font)` before `InitGraph`, and pass `''` as the path
  so `InitGraph` uses the linked drivers.
- Compile with `tpc` under 8.3 filenames; for FPC TP mode builds, keep the same
  register/init pattern but note that Graph availability depends on the target OS.

## BGI style tables
Constants below come from the standard `Graph` unit/BGI headers.

**Text fonts (`SetTextStyle` font parameter)**
| Constant         | Value | Notes                          |
|------------------|-------|--------------------------------|
| `DefaultFont`    | 0     | Built-in hardware font         |
| `TriplexFont`    | 1     | Outline font                   |
| `SmallFont`      | 2     | Small hardware font            |
| `SansSerifFont`  | 3     | Outline font                   |
| `GothicFont`     | 4     | Outline font                   |

**Text directions (`SetTextStyle` direction parameter)**
| Constant        | Value | Direction     |
|-----------------|-------|---------------|
| `HorizDir`      | 0     | Left-to-right |
| `VertDir`       | 1     | Bottom-to-top |

**Line styles (`SetLineStyle` style parameter)**
| Constant            | Value | Pattern          |
|---------------------|-------|------------------|
| `SolidLn`           | 0     | Solid            |
| `DottedLn`          | 1     | Dotted           |
| `CenterLn`          | 2     | Center           |
| `DashedLn`          | 3     | Dashed           |
| `UserBitLn`         | 4     | Use `SetUserCharSize`/`SetLineStyle` pattern |

**Line thickness (`SetLineStyle` width parameter)**
| Constant         | Value | Notes          |
|------------------|-------|----------------|
| `NormWidth`      | 1     | Default        |
| `ThickWidth`     | 3     | Thicker lines  |

**Fill styles (`SetFillStyle`)**
| Constant             | Value | Pattern      |
|----------------------|-------|--------------|
| `EmptyFill`          | 0     | Hollow       |
| `SolidFill`          | 1     | Solid        |
| `LineFill`           | 2     | Horizontal   |
| `LtSlashFill`        | 3     | Light /      |
| `SlashFill`          | 4     | Dense /      |
| `BkSlashFill`        | 5     | Dense \      |
| `LtBkSlashFill`      | 6     | Light \      |
| `HatchFill`          | 7     | Crosshatch   |
| `XHatchFill`         | 8     | Diag cross   |
| `InterleaveFill`     | 9     | Interleaved  |
| `WideDotFill`        | 10    | Wide dots    |
| `CloseDotFill`       | 11    | Close dots   |
| `UserFill`           | 12    | 8x8 bitmask via `SetFillPattern` |

## Notes
- Portable init/deinit: always check `GraphResult` after `InitGraph`; call `CloseGraph` on exit. Wrap in `{$IFDEF FPC}` guards when selecting DOS-only Graphics targets.
- Modern alternatives: SDL_BGI and WinBGI provide drop-in `Graph` APIs for modern OSes; note that behavior (fonts, colors) may differ. Consider using them for non-DOS builds.

## Modern Alternatives: SDL_BGI and WinBGI

For non-DOS targets, consider these drop-in BGI replacements:

### SDL_BGI
- **Project**: [SDL_BGI on GitHub](https://github.com/Rossano/SDL_bgi)
- **Platform**: Cross-platform (uses SDL2)
- **Compatibility**: Provides `Graph` unit API
- **Usage**: Link with SDL_BGI library instead of native BGI

### WinBGI
- **Platform**: Windows-specific
- **Compatibility**: Borland BGI API for Windows applications  
- **Usage**: Suitable for Win32 FPC targets

### Conditional Compilation Example
```pascal
{$IFDEF FPC}
  {$IFDEF WINDOWS}
uses WinGraph;  { WinBGI for Windows }
  {$ELSE}
uses Graph;     { SDL_BGI or native Graph }
  {$ENDIF}
{$ELSE}
uses Graph;     { TP7 native BGI }
{$ENDIF}
```

**Note**: Colors, fonts, and timing may differ slightly from original TP7 BGI. Test thoroughly on target platforms.
