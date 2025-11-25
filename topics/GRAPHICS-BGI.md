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

### Basic Shapes
Compatibility: TP5+, TP7 (BGI); FPC with Graph
```pascal
{$IFDEF FPC}{$MODE TP}{$ENDIF}
uses Crt, Graph;
var Gd, Gm: Integer;
begin
  Gd := Detect;
  InitGraph(Gd, Gm, '');
  if GraphResult <> grOk then Halt(1);
  
  { Lines }
  SetColor(White);
  Line(10, 10, 100, 100);      { Absolute line }
  MoveTo(100, 100);
  LineRel(50, 0);              { Relative from current position }
  LineTo(200, 50);             { Absolute to position }
  
  { Rectangles }
  SetColor(Yellow);
  Rectangle(50, 50, 150, 100);           { Outline only }
  SetFillStyle(SolidFill, Red);
  Bar(200, 50, 300, 100);                { Filled rectangle }
  Bar3D(350, 50, 450, 100, 10, TopOn);   { 3D bar }
  
  { Circles and Arcs }
  SetColor(Cyan);
  Circle(100, 200, 50);                  { Full circle }
  Arc(250, 200, 0, 180, 50);             { Half circle (0-180 degrees) }
  
  { Ellipses }
  SetColor(LightGreen);
  Ellipse(400, 200, 0, 360, 80, 40);     { Full ellipse }
  
  { Filled shapes }
  SetFillStyle(HatchFill, LightBlue);
  FillEllipse(100, 350, 60, 30);
  PieSlice(250, 350, 45, 135, 50);       { Pie slice 45-135 degrees }
  
  ReadKey;
  CloseGraph;
end.
```

**Common Drawing Functions:**
- `Line(x1,y1,x2,y2)` - Draw line between two points
- `Rectangle(x1,y1,x2,y2)` - Draw rectangle outline
- `Bar(x1,y1,x2,y2)` - Draw filled rectangle
- `Circle(x,y,radius)` - Draw circle
- `Arc(x,y,start,end,radius)` - Draw arc (angles in degrees)
- `Ellipse(x,y,start,end,xrad,yrad)` - Draw ellipse
- `FillEllipse(x,y,xrad,yrad)` - Draw filled ellipse
- `PieSlice(x,y,start,end,radius)` - Draw filled pie slice
- `FloodFill(x,y,border)` - Fill area bounded by border color

**Coordinate System:**
- Origin (0,0) is top-left corner
- X increases rightward, Y increases downward
- Use `GetMaxX()` and `GetMaxY()` for screen dimensions

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

## Color Constants

Standard BGI colors (4-bit palettes):

| Constant       | Value | RGB Equivalent | Constant       | Value | RGB Equivalent |
|----------------|-------|----------------|----------------|-------|----------------|
| `Black`        | 0     | (0,0,0)        | `DarkGray`     | 8     | (85,85,85)     |
| `Blue`         | 1     | (0,0,170)      | `LightBlue`    | 9     | (85,85,255)    |
| `Green`        | 2     | (0,170,0)      | `LightGreen`   | 10    | (85,255,85)    |
| `Cyan`         | 3     | (0,170,170)    | `LightCyan`    | 11    | (85,255,255)   |
| `Red`          | 4     | (170,0,0)      | `LightRed`     | 12    | (255,85,85)    |
| `Magenta`      | 5     | (170,0,170)    | `LightMagenta` | 13    | (255,85,255)   |
| `Brown`        | 6     | (170,85,0)     | `Yellow`       | 14    | (255,255,85)   |
| `LightGray`    | 7     | (170,170,170)  | `White`        | 15    | (255,255,255)  |

**Usage:**
```pascal
SetColor(Yellow);              { Drawing/line color }
SetBkColor(Blue);              { Background color }
SetFillStyle(SolidFill, Red);  { Fill color }
```

## Text Output

### Text in Graphics Mode
Compatibility: TP5+, TP7 (BGI); FPC with Graph
```pascal
{$IFDEF FPC}{$MODE TP}{$ENDIF}
uses Crt, Graph;
var Gd, Gm: Integer;
begin
  Gd := Detect;
  InitGraph(Gd, Gm, '');
  if GraphResult <> grOk then Halt(1);
  
  { Position and write text }
  SetColor(White);
  MoveTo(100, 50);
  OutText('Hello BGI!');
  
  { Direct positioning }
  OutTextXY(100, 100, 'Positioned text');
  
  { Styled text }
  SetTextStyle(TriplexFont, HorizDir, 4);
  SetColor(Yellow);
  OutTextXY(100, 150, 'Large Triplex');
  
  { Justified text }
  SetTextStyle(DefaultFont, HorizDir, 1);
  SetTextJustify(CenterText, CenterText);
  SetColor(LightGreen);
  OutTextXY(GetMaxX div 2, GetMaxY div 2, 'Centered');
  
  ReadKey;
  CloseGraph;
end.
```

**Text Justification Constants:**

| Horizontal    | Value | Vertical      | Value |
|---------------|-------|---------------|-------|
| `LeftText`    | 0     | `BottomText`  | 0     |
| `CenterText`  | 1     | `CenterText`  | 1     |
| `RightText`   | 2     | `TopText`     | 2     |

**Text Functions:**
- `OutText(s)` - Write text at current position
- `OutTextXY(x,y,s)` - Write text at specified position
- `SetTextStyle(font,direction,size)` - Set font, direction, and size
- `SetTextJustify(horiz,vert)` - Set text alignment
- `TextWidth(s)` - Get pixel width of string
- `TextHeight(s)` - Get pixel height of string

## Error Handling

Always check `GraphResult` after `InitGraph`:

```pascal
InitGraph(Gd, Gm, '');
case GraphResult of
  grOk:           { Success - continue };
  grNotDetected:  WriteLn('Graphics hardware not detected');
  grFileNotFound: WriteLn('BGI driver file not found');
  grInvalidDriver:WriteLn('Invalid driver file');
  grNoLoadMem:    WriteLn('Not enough memory to load driver');
  grNoScanMem:    WriteLn('Out of memory in scan fill');
  grNoFloodMem:   WriteLn('Out of memory in flood fill');
  grFontNotFound: WriteLn('Font file not found');
  grNoFontMem:    WriteLn('Not enough memory to load font');
  grInvalidMode:  WriteLn('Invalid graphics mode for selected driver');
  else WriteLn('Unknown graphics error: ', GraphResult);
end;
if GraphResult <> grOk then Halt(1);
```

**Common Error Solutions:**
- `grFileNotFound` - Check BGI path; use `''` for current directory or embed drivers with BINOBJ
- `grNoLoadMem` - Reduce memory usage or use embedded drivers (see BINOBJ section)
- `grInvalidMode` - Mode not supported by hardware; try `Detect` or different mode

**Helper Function:**
```pascal
ErrorMsg := GraphErrorMsg(GraphResult);  { Get error description string }
```

## Complete Example

Compatibility: TP5+, TP7 (BGI); FPC with Graph
```pascal
{$IFDEF FPC}{$MODE TP}{$ENDIF}
program BGIDemo;
uses Crt, Graph;

var
  Gd, Gm, ErrorCode: Integer;
  I: Integer;

begin
  { Initialize graphics }
  Gd := Detect;
  InitGraph(Gd, Gm, '');
  ErrorCode := GraphResult;
  
  if ErrorCode <> grOk then
  begin
    WriteLn('Graphics error: ', GraphErrorMsg(ErrorCode));
    WriteLn('Press any key...');
    ReadKey;
    Halt(1);
  end;
  
  { Clear screen and set background }
  ClearDevice;
  SetBkColor(Black);
  
  { Draw title }
  SetTextStyle(TriplexFont, HorizDir, 4);
  SetTextJustify(CenterText, TopText);
  SetColor(Yellow);
  OutTextXY(GetMaxX div 2, 10, 'BGI Demo');
  
  { Draw colored rectangles }
  SetTextStyle(DefaultFont, HorizDir, 1);
  SetTextJustify(LeftText, TopText);
  for I := 0 to 7 do
  begin
    SetFillStyle(SolidFill, I + 1);
    Bar(50 + I * 40, 80, 80 + I * 40, 120);
  end;
  SetColor(White);
  OutTextXY(50, 130, 'Color palette');
  
  { Draw concentric circles }
  SetColor(White);
  for I := 1 to 5 do
    Circle(GetMaxX div 2, 220, I * 15);
  OutTextXY(GetMaxX div 2 - 50, 300, 'Circles');
  
  { Draw filled shapes }
  SetFillStyle(HatchFill, LightBlue);
  FillEllipse(150, 380, 60, 40);
  
  SetFillStyle(SolidFill, LightGreen);
  PieSlice(GetMaxX div 2, 380, 0, 90, 50);
  
  SetFillStyle(SlashFill, LightRed);
  Bar3D(GetMaxX - 150, 340, GetMaxX - 80, 420, 15, TopOn);
  
  SetColor(White);
  OutTextXY(50, 430, 'Filled shapes');
  
  { Instructions }
  SetTextStyle(DefaultFont, HorizDir, 1);
  SetTextJustify(CenterText, BottomText);
  SetColor(LightGray);
  OutTextXY(GetMaxX div 2, GetMaxY - 10, 'Press any key to exit...');
  
  ReadKey;
  CloseGraph;
end.
```

This example demonstrates:
- Proper initialization and error handling
- Title with large font
- Color palette display
- Multiple drawing primitives (circles, ellipses, bars, pie slices)
- Different fill styles
- Text output with various styles and justification

## BGI Style Tables

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
- Portable init/deinit: always check `GraphResult` after `InitGraph`; call `CloseGraph` on exit.
- Wrap in `{$IFDEF FPC}` guards when selecting DOS-only Graphics targets.
- BGI is TP7/DOS-focused; FPC can use `Graph` unit on DOS targets.
- For modern OSes, FPC may require platform-specific graphics libraries or emulation layers.

