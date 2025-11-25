# Turbo Pascal 5.0 Notes

Guidance for generating code that targets Turbo Pascal 5.0 (1989). Units and BGI exist, but the object model and `asm` blocks are still absent (objects arrive in 5.5; `asm ... end` in 6.0). Cross-check switch syntax and unit availability with the original manual when emitting concrete build steps.

## Scope and language constraints
- Units are available (`uses` works), but the RTL is smaller than TP7. Confirm unit availability before referencing `Crt`, `Dos`, `Graph`, `Overlay`, or `Printer`.
- No `object` type or OOP features; treat any object-oriented code as TP7+/FPC-only.
- No `asm ... end`; use `Inline(...)` byte sequences when machine code is unavoidable.
- Strings are ShortString-style (length byte, 255 max). Managed strings and dynamic arrays do not exist.
- Types: `Integer` = 16-bit, `Real` = 6-byte by default; `LongInt` is 32-bit. Keep arithmetic within 16-bit limits unless `LongInt` is explicitly required.
- Filenames and paths must be 8.3; keep unit names short and avoid nested directories.
- Standard units shipped: `System`, `Crt`, `Dos`, `Graph`, `Printer`, plus compatibility shims (`Turbo3`, `Graph3`) and `Overlay` when compiled overlay-safe.

## Directives and switches (delta vs TP7)
- Switch directives and defaults: `$A+` align data (global), `$B-` short-circuit Boolean, `$D+` debug info, `$E+` 8087 emulation (effective when `$N+`), `$F-` auto NEAR/FAR (force FAR with `$F+`), `$I+` I/O checking, `$L+` local symbols, `$N-` software FP (use `$N+` for 8087), `$O-` overlays allowed off (turn on for overlay-safe codegen), `$R-` range checks off, `$S+` stack checks on, `$V+` strict var-string lengths. `$G` (286 opcodes) does not exist here; `$X` extended syntax remains available.
- Parameter directives: `$I filename` include (max 15 nested, not inside a statement part), `$L filename` link Intel .OBJ, `$M stack,heapmin,heapmax` (default `$M 16384,0,655360`, stack range 1024–65520), `$O UnitName` program-only overlay placement (after `uses`; unit must be compiled with `$O+`).
- Conditional directives: `$DEFINE/$UNDEF` symbols; `$IFDEF/$IFNDEF/$IFOPT/$ELSE/$ENDIF` with up to 16 nested levels. Built-in symbols include `VER50`, `MSDOS`, and `CPU86`.
- Missing/limited compared to TP7: no `$H` (ansi/shortstring toggle), no `$J` (writable typed const), no `$P` (open strings). Inline assembler is not available.
- Command-line options differ from TP7 `tpc`; use the TP5 syntax (`TPC` with `/$letter` switches or `TPC.CFG`).

### Directive defaults (from the TP5.0 manual)
| Directive | Default | Scope | Notes |
| --- | --- | --- | --- |
| `$A` | `+` | Global | Align data/typed consts on word boundary. |
| `$B` | `-` | Local | Short-circuit Boolean eval. |
| `$D` | `+` | Global | Debug info (line tables). |
| `$E` | `+` | Global | 8087 emulation when `$N+` (programs only). |
| `$F` | `-` | Local | Auto NEAR/FAR; `+` forces FAR (overlays/procvars). |
| `$I` | `+` | Local | I/O checking; `$I-` uses `IOResult`. |
| `$L` | `+` | Global | Local symbols (debugger/map). |
| `$N` | `-` | Global | Software FP; `+` uses 8087. |
| `$O` | `-` | Global | Overlay-safe codegen (unit must be compiled with `+` to overlay). |
| `$R` | `-` | Local | Range checks off. |
| `$S` | `+` | Local | Stack overflow checks. |
| `$V` | `+` | Local | Strict var-string lengths. |
| `$I filename` | — | Local | Include file (max 15 nested; not mid-statement). |
| `$L filename` | — | Local | Link Intel .OBJ. |
| `$M stack,heapmin,heapmax` | `16384,0,655360` | Global | Program stack/heap sizing (stack 1–65520). |
| `$O UnitName` | — | Local (program) | Mark unit for overlay; unit must be compiled with `$O+`. |
| Conditionals | — | — | `$DEFINE/$UNDEF`, `$IFDEF/$IFNDEF/$IFOPT/$ELSE/$ENDIF` (nest up to 16). |

## Command-line switches (TPC 5.0)
- Switch prefix `/`; set defaults via `/$<letter>{+|-}` (e.g., `/$R-,I-,O+,F+`).
- Paths: `/Ipath1;path2` (include search), `/Upath1;path2` (unit search), `/Opath1;path2` (OBJ search).
- Output: `/Epath` (EXE/TPU output directory), `/L` or `/G[S|P|O]` (map file variants), `/N` (embed standalone debugger info).
- Build: `/B` (build all), `/M` (make/recompile changed), `/Q` (quiet), `/Fseg:ofs` (find-error; needs `$D+/$L+`).
- Config: `TPC.CFG` is read automatically; source directives override CFG/CLI defaults.

## Units inventory (TP5)
- `System`: core types, math, memory (`New/Dispose/GetMem/FreeMem/Mark/Release`), `ExitProc`, random, pointer helpers, `RunError`, `ParamStr`, `ExitCode`.
- `Dos`: `FileMode`, `FindFirst/FindNext/DoneFind`, `Exec/SwapVectors`, date/time, disk info (`DiskFree/DiskSize`), interrupts (`MsDos/Intr`, `GetIntVec/SetIntVec`), environment (`GetEnv/EnvStr/EnvCount`), `DosError`, `PackTime/UnpackTime`.
- `Crt`: console I/O, colors/windows, `Delay`, `KeyPressed/ReadKey`, `CheckBreak`, `TextMode`, `ClrScr`, cursor positioning, simple `Sound/NoSound`.
- `Graph`: BGI API (drivers/fonts via `.BGI/.CHR` or `BINOBJ` + `RegisterBGIdriver/RegisterBGIfont`), drawing/text primitives, viewports, palettes/modes (fewer drivers/modes than TP7).
- `Printer`: `AssignPrn`, `WriteLn`, `Flush`.
- Compatibility: `Turbo3` (`Kbd`, `CBreak` helpers) and `Graph3` (legacy graph helpers) for TP3-era code.
- `Overlay`: overlay manager when units compiled with `$O+` and program lists `{$O Unit}` and calls `OvrInit`.

## Reserved words and types (TP5)
- Reserved words (ANSI set plus `string`): `and array begin case const div do downto else end file for function goto if in label mod nil not of or packed procedure program record repeat set then to type until var while with string`.
- Standard types:

| Type | Size/Notes |
| --- | --- |
| `Boolean` | 1 byte (FALSE/TRUE) |
| `Char` | 1 byte |
| `Byte` | 1 byte unsigned |
| `Integer` | 2 bytes |
| `Word` | 2 bytes unsigned |
| `LongInt` | 4 bytes |
| `Real` | 6-byte Real48 (software FP) |
| `Single/Double/Extended/Comp` | 8087 FP types when `$N+` (with `$E+` emulation) |
| `ShortString` | 255 max |
| `Text`, `File`, `file of ...` | File types |
- ANSI deviations (manual Appendix A): `Get/Put` replaced by `Read/Write`; `Page` not implemented; `goto` cannot leave current block; `packed` accepted but auto-packing used; procedural parameters limited; case with no matching selector is ignored unless `else` present; `string` is a TP extension.

## RTL/library expectations
- Available units typically include `System`, `Dos`, `Crt`, `Graph`, `Printer`, `Turbo3`/compat shims, and `Overlay` (earlier overlay manager). Coverage is narrower than TP7; avoid citing TP7-only routines.
- BGI is present but matches the era: fewer drivers/modes than TP7; verify constants and init patterns before emitting code.
- No SysUtils-style exceptions; runtime errors use numeric codes.

## Memory model and limits
- 16-bit real-mode with 64KB segment limits; keep global data and code size constrained.
- Overlays exist but with fewer controls than TP7; expect to compile overlay-ready units with `$O+/$F+` and use the `Overlay` unit when available.
- Floating point: software by default; `$N+` targets 8087 hardware.

## Code generation guidelines
- Keep code TP5-safe: units allowed, but avoid OOP and `asm` blocks.
- Mark overlays explicitly only if the target toolchain supports them; otherwise keep code small and split units.
- Prefer `LongInt` only where needed; otherwise stick to 16-bit arithmetic.
- Retain TP-style ShortString handling and text/typed/untyped file patterns; avoid managed types and FPC/TP7-only directives.

## Compiler directives (quick lookup)
- `$A+/-` Align data (word-align vars/typed consts; default on).
- `$B+/-` Boolean evaluation (default short-circuit).
- `$D+/-` Debug info (line tables; required for Find Error/debugger).
- `$E+/-` 8087 emulation when `$N+` (default on; ignored if `$N-`).
- `$F+/-` Force FAR calls (default off; use `+` for overlays/procedural vars).
- `$I+/-` I/O checking (default on; `$I-` requires `IOResult`).
- `$L+/-` Local symbols (default on; useful with `$D+` for debugging/map).
- `$N+/-` Numeric processing (default off; `+` uses 8087).
- `$O+/-` Overlays allowed (default off; `+` makes code overlay-safe).
- `$R+/-` Range checking (default off).
- `$S+/-` Stack checking (default on).
- `$V+/-` Var-string strictness (default on).
- `$I filename` Include file; `$L filename` link OBJ; `$M stack,heapmin,heapmax` memory sizes (default `16384,0,655360`); `$O UnitName` overlay placement in programs.
- Conditional: `$DEFINE/$UNDEF`, `$IFDEF/$IFNDEF/$IFOPT/$ELSE/$ENDIF` (nest up to 16).

## Run-time errors (decimal)
- DOS errors (1–17) include: 1 invalid function, 2 file not found, 3 path not found, 4 too many open files (max 15), 5 access denied, 6 invalid handle, 12 invalid FileMode, 15 invalid drive, 16 cannot remove current dir, 17 cannot rename across drives.
- I/O errors (100–106, only if `$I+`): 100 disk read error, 101 disk write error, 102 file not assigned, 103 file not open, 104 file not open for input, 105 file not open for output, 106 invalid numeric format.
- Critical errors (150–162): write-protect, unknown unit, drive not ready, CRC/seek/media/printer/device faults, hardware failure.
- Fatal errors (200+): 200 division by zero, 201 range check (`$R+`), 202 stack overflow (`$S+`), 203 heap overflow, 204 invalid pointer operation, 205 floating-point overflow, 206 floating-point underflow (unmasked 8087), 207 invalid floating-point operation (Trunc/Round range, sqrt/ln domain, 8087 stack overflow), 208 overlay manager not installed, 209 overlay file read error.
- Notes (per manual):
  - DOS errors map to Reset/Rewrite/Append/Erase/Rename/ChDir/MkDir/RmDir when paths/handles are invalid; open-file cap is 15 unless CONFIG.SYS `FILES=` is higher.
  - I/O errors surface only when `$I+`; with `$I-`, code must call `IOResult` after each I/O.
  - Fatal errors terminate immediately; `Find Error` works when compiled with `$D+/$L+` and map/debug info is present.

## Units (availability highlights)
- `System`: core types, memory manager, math, `ExitProc/RunError`, `ParamStr/ExitCode`.
- `Dos`: FileMode, `FindFirst/FindNext`, `Exec/SwapVectors`, date/time, `GetIntVec/SetIntVec`, device/drive helpers.
- `Crt`: console I/O (colors, modes, `Delay`, `KeyPressed/ReadKey`, `TextMode`).
- `Graph`: BGI API; drivers/fonts via `.BGI/.CHR` or `BINOBJ` + `RegisterBGIdriver/RegisterBGIfont`.
- `Printer`: `AssignPrn` + `WriteLn`.
- Compat: `Turbo3` and `Graph3` for TP3-era code; `Overlay` unit when building overlay-safe programs.
